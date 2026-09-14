;;; org-onstate-test.el --- Public behavior tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'org-onstate)
(require 'org-onstate-actions)

(defvar org-state)
(defvar org-last-state)
(defvar osa-test-source nil)
(defvar osa-test-target nil)
(defvar osa-test-leaf nil)
(defvar osa-test-log nil)
(defvar osa-test-warnings nil)
(defvar osa-test-now nil)

(defun osa-test-note (&rest values)
  (setq osa-test-log (append osa-test-log (list values))))

(defun osa-test-warning (_original &rest arguments)
  ;; Observe Emacs's warning boundary, without replacing any Org operation.
  (push (format "%S" arguments) osa-test-warnings))

(defun osa-test-events (label)
  (let (events)
    (dolist (event osa-test-log (nreverse events))
      (when (eq (car event) label) (push event events)))))

(defun osa-test-call-with-fixture (body)
  (let* ((directory (make-temp-file "org-onstate-test-" t))
         (org-agenda-files (mapcar (lambda (name) (expand-file-name name directory))
                                  '("source.org" "target.org" "leaf.org")))
         (org-id-locations-file (expand-file-name "ids" directory))
         (org-id-locations nil) (org-id-files nil) (org-id-extra-files nil)
         (org-id-track-globally t)
         (org-agenda-buffer-name (concat "*osa-test-" (file-name-nondirectory directory) "*"))
         (org-agenda-start-on-weekday nil) (org-agenda-sticky nil)
         (org-agenda-include-diary nil) (org-agenda-window-setup 'current-window)
         (org-todo-keywords '((sequence "TODO" "WAIT" "|" "DONE")))
         (org-todo-repeat-to-state "TODO")
         (org-log-done nil) (org-log-repeat nil)
         (org-element-use-cache nil) (org-startup-folded nil)
         (org-use-property-inheritance nil)
         (org-after-todo-state-change-hook nil)
         (make-backup-files nil) (auto-save-default nil) (create-lockfiles nil)
         (osa-test-log nil) (osa-test-warnings nil)
         osa-test-source osa-test-target osa-test-leaf)
    (unwind-protect
        (progn
          (let ((texts '("* Parent\n** TODO Source\n:PROPERTIES:\n:ID: trimmer-start\n:END:\n"
                         "* DONE Target\n:PROPERTIES:\n:ID: trimmer-finish\n:END:\n"
                         "* TODO Leaf\n:PROPERTIES:\n:ID: leaf\n:END:\n")))
            (dolist (file org-agenda-files)
              (with-current-buffer (find-file-noselect file)
                (insert (pop texts))
                (org-mode)
                (save-buffer))))
          (org-id-update-id-locations org-agenda-files t)
          ;; Force a real lookup through the isolated persisted ID index.
          (setq org-id-locations nil)
          (setq osa-test-source (org-id-find "trimmer-start" t)
                osa-test-target (org-id-find "trimmer-finish" t)
                osa-test-leaf (org-id-find "leaf" t))
          (advice-add 'display-warning :around #'osa-test-warning)
          (funcall body))
      (advice-remove 'display-warning #'osa-test-warning)
      (when (get-buffer org-agenda-buffer-name) (kill-buffer org-agenda-buffer-name))
      (dolist (file org-agenda-files)
        (when-let ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (dolist (marker (list osa-test-source osa-test-target osa-test-leaf))
        (when marker (set-marker marker nil)))
      (delete-directory directory t))))

(defmacro osa-test-with-fixture (&rest body)
  (declare (indent 0) (debug t))
  `(osa-test-call-with-fixture (lambda () ,@body)))

(defun osa-test-enable (marker)
  (org-with-point-at marker (org-onstate-mode 1)))

(defun osa-test-repeat (marker)
  (org-with-point-at marker
    ;; Without brackets Org treats TIME as a date, dropping its repeater.
    (org-schedule nil "<2026-09-14 Mon .+1w>")
    (should (equal (org-get-repeat) ".+1w"))))

(defun osa-test-rule (marker state form)
  (org-with-point-at marker
    (org-entry-put nil (concat "ON_" state) (prin1-to-string form))))

(defun osa-test-change (id state)
  (let ((marker (org-id-find id t)))
    (unwind-protect (org-with-point-at marker (org-todo state))
      (set-marker marker nil))))

(defun osa-test-observe (label)
  (osa-test-note label org-last-state org-state (org-get-todo-state)
                 (org-entry-get nil "SCHEDULED")))

(defun osa-test-reactivate (&rest options)
  (osa-test-note 'reactivate)
  (apply #'org-onstate-schedule options))

(defun osa-test-source-context ()
  (should (eq (current-buffer) (marker-buffer osa-test-source)))
  (should (= (point) (marker-position osa-test-source)))
  (should (equal (org-entry-get nil "ID") "trimmer-start"))
  (osa-test-note 'context))

(defun osa-test-delay (marker start end seconds)
  (let ((scheduled (org-with-point-at marker
                     (float-time (org-time-string-to-time
                                  (org-entry-get nil "SCHEDULED"))))))
    ;; Minute-precision bounds, including a minute boundary during the call.
    (should (<= (* 60 (floor (/ (+ (float-time start) seconds) 60))) scheduled))
    (should (<= scheduled (* 60 (floor (/ (+ (float-time end) seconds) 60)))))))

(defun osa-test-complete (agenda)
  (if (not agenda)
      (org-with-point-at osa-test-source (org-todo "DONE"))
    (org-agenda-list nil (org-with-point-at osa-test-source
                          (substring (org-entry-get nil "SCHEDULED") 1 11)) 1)
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (setq found (equal (org-get-at-bol 'org-hd-marker) osa-test-source))
        (unless found (forward-line 1)))
      (should found)
      (should (derived-mode-p 'org-agenda-mode))
      (should (equal (org-with-point-at (org-get-at-bol 'org-marker)
                       (org-entry-get nil "ID")) "trimmer-start"))
      (org-agenda-todo 3))))

(defun osa-test-trimmer (multiple agenda)
  (osa-test-with-fixture
    (osa-test-enable osa-test-source)
    (osa-test-enable osa-test-target)
    (osa-test-rule osa-test-source "DONE"
                   (if multiple
                       '((osa-test-reactivate :id "trimmer-finish" :after "45m" :state "TODO")
                         (osa-test-source-context))
                     '(osa-test-reactivate :id "trimmer-finish" :after "45m" :state "TODO")))
    (osa-test-rule osa-test-source "TODO" '(osa-test-observe reset))
    (osa-test-rule osa-test-target "TODO" '(osa-test-observe target))
    (osa-test-repeat osa-test-source)
    (dotimes (round 2)
      (when (= round 1)
        (osa-test-change "trimmer-finish" "DONE")
        (org-with-point-at osa-test-target (org-schedule nil "2000-01-01 Sat 00:00")))
      (let ((old (org-with-point-at osa-test-source (org-entry-get nil "SCHEDULED")))
            (day (+ (org-today) 7))
            (start (current-time)))
        (osa-test-complete agenda)
        (osa-test-delay osa-test-target start (current-time) 2700)
        (org-with-point-at osa-test-source
          (should (equal (org-get-todo-state) "TODO"))
          (should (equal (org-get-repeat) ".+1w"))
          (should (= day (time-to-days (org-time-string-to-time
                                       (org-entry-get nil "SCHEDULED"))))))
        (should (equal (car (last (osa-test-events 'reset)))
                       (list 'reset "DONE" "TODO" "TODO" old))))
      (should (= (length (osa-test-events 'reactivate)) (1+ round)))
      (should (= (length (osa-test-events 'context)) (if multiple (1+ round) 0))))
    (should (= (length (osa-test-events 'target)) 2))
    (dolist (event (osa-test-events 'target))
      (should (equal (butlast event) '(target "DONE" "TODO" "TODO")))
      (should (string-match-p "[0-9][0-9]:[0-9][0-9]" (car (last event)))))
    (org-with-point-at osa-test-target
      (should (equal (org-get-todo-state) "TODO"))
      (should-not (org-get-repeat)))
    (let ((count 0) (headings 0))
      (dolist (file org-agenda-files)
        (with-current-buffer (get-file-buffer file)
          (org-map-entries (lambda ()
                             (when (equal (org-get-heading t t t t) "Target")
                               (setq headings (1+ headings)))
                             (when (equal (org-entry-get nil "ID") "trimmer-finish")
                               (setq count (1+ count)))))))
      (should (= count 1))
      (should (= headings 1)))
    (should-not osa-test-warnings)))

(ert-deftest osa-single-direct () (osa-test-trimmer nil nil))
(ert-deftest osa-multiple-direct () (osa-test-trimmer t nil))
(ert-deftest osa-single-agenda () (osa-test-trimmer nil t))
(ert-deftest osa-multiple-agenda () (osa-test-trimmer t t))

(ert-deftest osa-mode-is-local-opt-in-and-removable ()
  (should (custom-variable-p 'org-onstate-max-events))
  (should (= (default-value 'org-onstate-max-events) 100))
  (with-temp-buffer (should-error (org-onstate-mode 1)))
  (osa-test-with-fixture
    (osa-test-rule osa-test-source "DONE" '(osa-test-note source))
    (osa-test-rule osa-test-target "TODO" '(osa-test-note target))
    (osa-test-change "trimmer-start" "DONE")
    (should-not osa-test-log)
    (osa-test-enable osa-test-source)
    (osa-test-enable osa-test-source)
    (osa-test-change "trimmer-start" "TODO")
    (osa-test-change "trimmer-start" "DONE")
    (osa-test-change "trimmer-finish" "TODO")
    (should (equal osa-test-log '((source))))
    (org-with-point-at osa-test-source (org-onstate-mode -1))
    (osa-test-change "trimmer-start" "TODO")
    (osa-test-change "trimmer-start" "DONE")
    (should (equal osa-test-log '((source))))))

(ert-deftest osa-exact-local-state-ignores-inheritance-no-change-and-removal ()
  (osa-test-with-fixture
    (let ((org-use-property-inheritance t))
      (org-with-point-at osa-test-source
        (org-up-heading-safe)
        (org-entry-put nil "ON_DONE" "(osa-test-note inherited)"))
      (osa-test-enable osa-test-source)
      (osa-test-change "trimmer-start" "DONE")
      (should-not osa-test-log)
      (osa-test-rule osa-test-source "WAIT" '(osa-test-observe waiting))
      (osa-test-change "trimmer-start" "WAIT")
      (osa-test-change "trimmer-start" "WAIT")
      (osa-test-change "trimmer-start" 'none)
      (should (equal osa-test-log '((waiting "DONE" "WAIT" "WAIT" nil))))
      (should-not osa-test-warnings))))

(ert-deftest osa-empty-actions-and-literal-arguments ()
  (osa-test-with-fixture
    (osa-test-enable osa-test-source)
    (dolist (property '("" "nil" "()" "   "))
      (org-with-point-at osa-test-source (org-entry-put nil "ON_DONE" property))
      (osa-test-change "trimmer-start" "DONE")
      (osa-test-change "trimmer-start" "TODO"))
    (should-not osa-test-log)
    (should-not osa-test-warnings)
    (osa-test-rule osa-test-source "DONE"
                   '(osa-test-note :data (progn (error "must not evaluate")) symbol [one two]))
    (osa-test-change "trimmer-start" "DONE")
    (should (equal osa-test-log
                   '((:data (progn (error "must not evaluate")) symbol [one two]))))))

(ert-deftest osa-invalid-forms-validate-entire-list-before-effects ()
  (dolist (property '("(" "(osa-test-note first) trailing" "(osa-test-note first) (ignore)"
                      "(osa-test-note first) ; trailing comment"
                      "(osa-test-note . tail)" "#1=(osa-test-note . #1#)"
                      "#1=((osa-test-note first) . #1#)"
                      "((osa-test-note first) (osa-test-note . tail))"
                      "((osa-test-note first) (osa-test-missing-function))"
                      "((osa-test-note first) (when t (ignore)))"
                      "((osa-test-note first) (if t 1 2))"
                      "((lambda () (ignore)))" "42"))
    (ert-info ((format "Property: %s" property))
      (osa-test-with-fixture
        (osa-test-enable osa-test-source)
        (org-with-point-at osa-test-source (org-entry-put nil "ON_DONE" property))
        (osa-test-change "trimmer-start" "DONE")
        (should-not osa-test-log)
        (should osa-test-warnings)
        (let ((warning (mapconcat #'identity osa-test-warnings "\n")))
          (should (string-match-p "source.org" warning))
          (should (string-match-p "ON_DONE" warning)))
        (org-with-point-at osa-test-source (should (equal (org-get-todo-state) "DONE")))))))

(ert-deftest osa-noncallable-function-cell-is-rejected-before-any-action ()
  (unwind-protect
      (progn
        (fset 'osa-test-noncallable 42)
        (osa-test-with-fixture
          (osa-test-enable osa-test-source)
          (osa-test-rule osa-test-source "DONE"
                         '((osa-test-note must-not-run) (osa-test-noncallable)))
          (osa-test-change "trimmer-start" "DONE")
          (should-not osa-test-log)
          (should osa-test-warnings)))
    (fmakunbound 'osa-test-noncallable)))

(defun osa-test-disturb-context ()
  (widen)
  (goto-char (point-min))
  (insert "* Inserted before source\n")
  (narrow-to-region (point-max) (point-max))
  (set-buffer (marker-buffer osa-test-target))
  (goto-char (point-max)))

(ert-deftest osa-actions-restore-context-and-follow-moving-source-marker ()
  (osa-test-with-fixture
    (osa-test-enable osa-test-source)
    (osa-test-rule osa-test-source "DONE"
                   '((osa-test-disturb-context) (osa-test-source-context)))
    (with-current-buffer (marker-buffer osa-test-source)
      (goto-char osa-test-source)
      (org-narrow-to-subtree)
      (let ((start (copy-marker (point-min))) (end (copy-marker (point-max) t)))
        (unwind-protect
            (progn
              (org-todo "DONE")
              (should (eq (current-buffer) (marker-buffer osa-test-source)))
              (should (= (point-min) start))
              (should (= (point-max) end))
              (should (= (point) osa-test-source)))
          (set-marker start nil)
          (set-marker end nil))))
    (should (equal osa-test-log '((context))))
    (should-not osa-test-warnings)))

(defun osa-test-replace-leaf-rule ()
  (osa-test-rule osa-test-leaf "WAIT" '(osa-test-note replacement)))

(ert-deftest osa-fifo-multihop-preserves-selected-rule-and-event-states ()
  (osa-test-with-fixture
    (mapc #'osa-test-enable (list osa-test-source osa-test-target osa-test-leaf))
    (osa-test-rule osa-test-source "DONE"
                   '((osa-test-note a1) (osa-test-change "trimmer-finish" "TODO")
                     (osa-test-note a2) (osa-test-change "leaf" "WAIT") (osa-test-note a3)))
    (osa-test-rule osa-test-target "TODO"
                   '((osa-test-note b1) (osa-test-replace-leaf-rule)
                     (osa-test-change "leaf" "DONE") (osa-test-note b2)))
    (osa-test-rule osa-test-leaf "WAIT" '(osa-test-observe c))
    (osa-test-rule osa-test-leaf "DONE" '(osa-test-note d))
    (osa-test-change "trimmer-start" "DONE")
    (should (equal osa-test-log
                   '((a1) (a2) (a3) (b1) (b2) (c "TODO" "WAIT" "DONE" nil) (d))))
    (should-not osa-test-warnings)))

(defun osa-test-failure (nested)
  (osa-test-with-fixture
    (mapc #'osa-test-enable (list osa-test-source osa-test-target osa-test-leaf))
    (osa-test-repeat osa-test-source)
    (osa-test-rule osa-test-source "TODO" '(osa-test-note reset))
    (osa-test-rule osa-test-source "DONE"
                   (if nested
                       '((osa-test-change "trimmer-finish" "TODO")
                         (osa-test-change "leaf" "WAIT") (osa-test-note a-finished))
                     '((osa-test-change "trimmer-finish" "TODO")
                       (osa-test-note effect) (error "source boom") (osa-test-note after))))
    (osa-test-rule osa-test-target "TODO"
                   (if nested '((osa-test-note b-start) (error "nested boom") (osa-test-note after))
                     '(osa-test-note pending)))
    (osa-test-rule osa-test-leaf "WAIT" '(osa-test-note pending))
    (osa-test-change "trimmer-start" "DONE")
    (should (equal osa-test-log (if nested '((a-finished) (b-start) (reset))
                                 '((effect) (reset)))))
    (should osa-test-warnings)
    (let ((warning (mapconcat #'identity osa-test-warnings "\n")))
      (should (string-match-p (if nested "target.org" "source.org") warning))
      (should (string-match-p (if nested "ON_TODO" "ON_DONE") warning))
      (should (string-match-p "boom" warning)))
    (org-with-point-at osa-test-source
      (should (equal (org-get-todo-state) "TODO"))
      (should (equal (org-get-repeat) ".+1w"))
      (should (= (+ (org-today) 7) (time-to-days (org-time-string-to-time
                                               (org-entry-get nil "SCHEDULED"))))))
    (org-with-point-at osa-test-target (should (equal (org-get-todo-state) "TODO")))
    (osa-test-rule osa-test-leaf "DONE" '(osa-test-note fresh))
    (osa-test-change "leaf" "DONE")
    (should (equal (car (last osa-test-log)) '(fresh)))
    (should-not (osa-test-events 'pending))))

(ert-deftest osa-action-error-clears-pending-and-allows-repeat () (osa-test-failure nil))
(ert-deftest osa-nested-error-clears-pending-and-allows-repeat () (osa-test-failure t))

(ert-deftest osa-nested-observation-error-stops-current-event ()
  (osa-test-with-fixture
    (mapc #'osa-test-enable (list osa-test-source osa-test-target osa-test-leaf))
    (osa-test-repeat osa-test-source)
    (osa-test-rule osa-test-source "TODO" '(osa-test-note reset))
    (osa-test-rule osa-test-source "DONE"
                   '((osa-test-change "leaf" "WAIT")
                     (osa-test-change "trimmer-finish" "TODO") (osa-test-note after)))
    (osa-test-rule osa-test-leaf "WAIT" '(osa-test-note pending))
    (osa-test-rule osa-test-target "TODO" '(osa-test-missing-function))
    (osa-test-change "trimmer-start" "DONE")
    (should (equal osa-test-log '((reset))))
    (should osa-test-warnings)
    (org-with-point-at osa-test-source (should (equal (org-get-todo-state) "TODO")))))

(ert-deftest osa-cycle-limit-clears-chain-and-allows-next-event ()
  (osa-test-with-fixture
    (let ((org-onstate-max-events 5))
      (osa-test-enable osa-test-source)
      (osa-test-enable osa-test-target)
      (osa-test-rule osa-test-source "DONE"
                     '((osa-test-note event) (osa-test-change "trimmer-finish" "TODO")))
      (osa-test-rule osa-test-target "TODO"
                     '((osa-test-note event) (osa-test-change "trimmer-start" "TODO")))
      (osa-test-rule osa-test-source "TODO"
                     '((osa-test-note event) (osa-test-change "trimmer-finish" "DONE")))
      (osa-test-rule osa-test-target "DONE"
                     '((osa-test-note event) (osa-test-change "trimmer-start" "DONE")))
      (osa-test-change "trimmer-start" "DONE")
      (should (= (length osa-test-log) 5))
      (should osa-test-warnings)
      (osa-test-rule osa-test-source "WAIT" '(osa-test-note fresh))
      (osa-test-change "trimmer-start" "WAIT")
      (should (equal (car (last osa-test-log)) '(fresh)))
      (should (= (length osa-test-log) 6)))))

(ert-deftest osa-helper-rejects-invalid-options-before-editing ()
  (osa-test-with-fixture
    (let ((before (org-with-point-at osa-test-target (buffer-string))))
      (dolist (options '(nil (:id "trimmer-finish") (:after "45m")
                         (:id "" :after "45m") (:id 12 :after "45m")
                         (:id "trimmer-finish" :after "0m")
                         (:id "trimmer-finish" :after "-1m")
                         (:id "trimmer-finish" :after "1.5h")
                         (:id "trimmer-finish" :after "1w")
                         (:id "trimmer-finish" :after "45m trailing")
                         (:id "trimmer-finish" :after 45)
                         (:id "trimmer-finish" :after "45m" :state nil)
                         (:id "trimmer-finish" :after "45m" :state "UNKNOWN")
                         (:id "trimmer-finish" :after "45m" :state "todo")
                         (:id "trimmer-finish" :after "45m" :extra t)
                         (:id "trimmer-finish" :after "45m" :state)
                         (:id "trimmer-finish" :after "45m" :id "trimmer-finish")
                         (:id "trimmer-finish" :after "45m" :after "2h")
                         (:id "trimmer-finish" :after "45m" :state "TODO" :state "TODO")
                         (id "trimmer-finish" :after "45m")))
        (ert-info ((format "Options: %S" options))
          (should-error (apply #'org-onstate-schedule options))
          (should (equal before (org-with-point-at osa-test-target (buffer-string)))))))))

(ert-deftest osa-helper-rejects-missing-repeating-and-read-only-targets ()
  (osa-test-with-fixture
    (should-error (org-onstate-schedule :id "absent" :after "45m"))
    (osa-test-repeat osa-test-target)
    (let ((before (org-with-point-at osa-test-target (buffer-string))))
      (should-error (org-onstate-schedule :id "trimmer-finish" :after "45m" :state "TODO"))
      (should (equal before (org-with-point-at osa-test-target (buffer-string)))))
    (org-with-point-at osa-test-target
      (org-schedule '(4))
      (setq buffer-read-only t))
    (should-error (org-onstate-schedule :id "trimmer-finish" :after "45m" :state "TODO"))))

(ert-deftest osa-helper-preserves-state-skips-same-state-and-does-not-save ()
  (osa-test-with-fixture
    (osa-test-rule osa-test-target "TODO" '(osa-test-note target-action))
    (org-with-point-at osa-test-target
      (add-hook 'org-after-todo-state-change-hook
                (lambda () (osa-test-observe 'raw-hook)) nil t))
    (dolist (delay '(("45m" . 2700) ("2h" . 7200) ("1d" . 86400)))
      (let ((start (current-time)))
        (org-onstate-schedule :id "trimmer-finish" :after (car delay))
        (osa-test-delay osa-test-target start (current-time) (cdr delay))))
    (org-onstate-schedule :id "trimmer-finish" :after "45m" :state "DONE")
    (should-not osa-test-log)
    (org-with-point-at osa-test-target (should (equal (org-get-todo-state) "DONE")))
    (org-onstate-schedule :id "trimmer-finish" :after "45m" :state "TODO")
    (should (= (length (osa-test-events 'raw-hook)) 1))
    (should-not (osa-test-events 'target-action))
    (org-onstate-schedule :id "trimmer-finish" :after "45m" :state "WAIT")
    (org-with-point-at osa-test-target (should (equal (org-get-todo-state) "WAIT")))
    (should (= (length (osa-test-events 'raw-hook)) 2))
    (org-with-point-at osa-test-target
      (should (buffer-modified-p))
      (let ((file (buffer-file-name)))
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "^\\* DONE Target" (buffer-string)))
          (should-not (string-match-p "SCHEDULED:" (buffer-string))))))))

(defun osa-test-clock () osa-test-now)
(defun osa-test-advance-clock ()
  (setq osa-test-now (time-add osa-test-now (seconds-to-time 7200))))

(ert-deftest osa-schedule-uses-captured-event-time ()
  ;; Only the wall clock is controlled here; Org and package operations are real.
  ;; This makes a two-hour difference observable without a minute-long sleep.
  (osa-test-with-fixture
    (let ((osa-test-now (encode-time 0 0 10 13 9 2026)))
      (osa-test-enable osa-test-source)
      (osa-test-rule osa-test-source "DONE"
                     '((osa-test-advance-clock)
                       (org-onstate-schedule :id "trimmer-finish" :after "45m")))
      (advice-add 'current-time :override #'osa-test-clock)
      (unwind-protect
          (progn
            (osa-test-change "trimmer-start" "DONE")
            (org-with-point-at osa-test-target
              (should (string-match-p "2026-09-13 .* 10:45"
                                      (org-entry-get nil "SCHEDULED"))))
            (org-onstate-schedule :id "trimmer-finish" :after "2h")
            (org-with-point-at osa-test-target
              (should (string-match-p "2026-09-13 .* 14:00"
                                      (org-entry-get nil "SCHEDULED")))))
        (advice-remove 'current-time #'osa-test-clock)))))

(ert-deftest osa-quit-is-not-swallowed ()
  (osa-test-with-fixture
    (osa-test-enable osa-test-source)
    (osa-test-rule osa-test-source "DONE" '(signal quit nil))
    (let ((caught nil))
      (condition-case nil
          (osa-test-change "trimmer-start" "DONE")
        (quit (setq caught t)))
      (should caught)
      (should-not osa-test-warnings))))

(defun osa-test-delete-subtree (&optional id)
  (let ((marker (and id (org-id-find id t))))
    (unwind-protect
        (org-with-point-at (or marker (point))
          (org-back-to-heading t)
          (delete-region (point) (save-excursion (org-end-of-subtree t t) (point))))
      (when marker (set-marker marker nil)))))

(defun osa-test-damage-current-heading ()
  (osa-test-note 'wrong-heading (org-get-heading t t t t) org-state)
  (org-entry-put nil "CORRUPTED" "yes"))

(defun osa-test-deleted-source (queued)
  (osa-test-with-fixture
    (let* ((event-source (if queued osa-test-target osa-test-source))
           (victim (org-with-point-at event-source
                     (let ((level (org-outline-level)))
                       (goto-char (point-max))
                       (prog1 (point-marker)
                         (insert (make-string level ?*) " TODO Victim\n")))))
           (untouched (org-with-point-at victim
                        (buffer-substring-no-properties (point) (point-max)))))
      (unwind-protect
          (progn
            (osa-test-enable osa-test-source)
            (osa-test-enable osa-test-target)
            (osa-test-rule osa-test-source "DONE"
                           (if queued
                               '((osa-test-change "trimmer-finish" "TODO")
                                 (osa-test-delete-subtree "trimmer-finish"))
                             '((osa-test-delete-subtree) (osa-test-damage-current-heading))))
            (when queued
              (osa-test-rule osa-test-target "TODO" '(osa-test-damage-current-heading)))
            (osa-test-change "trimmer-start" "DONE")
            ;; A surviving marker must not redirect an old event onto Victim.
            (should-not osa-test-log)
            (should (equal untouched (org-with-point-at victim
                                       (buffer-substring-no-properties (point) (point-max)))))
            (should osa-test-warnings)
            (let ((warning (mapconcat #'identity osa-test-warnings "\n")))
              (should (string-match-p (if queued "target.org" "source.org") warning))
              (should (string-match-p (if queued "ON_TODO" "ON_DONE") warning))))
        (set-marker victim nil)))))

(ert-deftest osa-deleted-current-source-does-not-retarget-next-action ()
  (osa-test-deleted-source nil))

(ert-deftest osa-deleted-queued-source-does-not-retarget-event ()
  (osa-test-deleted-source t))

(defun osa-test-complete-target-and-continue ()
  (osa-test-change "trimmer-finish" "DONE")
  ;; A failed nested observation cannot preempt arbitrary function statements.
  (osa-test-note 'inside-function-continued)
  (osa-test-change "leaf" "DONE"))

(ert-deftest osa-nested-observation-error-allows-both-repeaters-to-finish ()
  (osa-test-with-fixture
    (osa-test-change "trimmer-finish" "TODO")
    (osa-test-repeat osa-test-source)
    (osa-test-repeat osa-test-target)
    (mapc #'osa-test-enable (list osa-test-source osa-test-target osa-test-leaf))
    (osa-test-rule osa-test-source "DONE"
                   '((osa-test-change "leaf" "WAIT")
                     (osa-test-complete-target-and-continue) (osa-test-note forbidden-action)))
    (osa-test-rule osa-test-source "TODO" '(osa-test-note outer-reset))
    (osa-test-rule osa-test-target "DONE" '(osa-test-missing-function))
    ;; These later observations must be ignored once the original failure occurs.
    (osa-test-rule osa-test-target "TODO" '(osa-test-reset-missing-function))
    (osa-test-rule osa-test-leaf "WAIT" '(osa-test-note forbidden-pending))
    (osa-test-rule osa-test-leaf "DONE" '(osa-test-secondary-missing-function))
    (let ((next-day (+ (org-today) 7)))
      (osa-test-change "trimmer-start" "DONE")
      (dolist (marker (list osa-test-source osa-test-target))
        (org-with-point-at marker
          (ert-info ((buffer-file-name))
            (should (equal (org-get-todo-state) "TODO"))
            (should (equal (org-get-repeat) ".+1w"))
            (should (= next-day (time-to-days (org-time-string-to-time
                                              (org-entry-get nil "SCHEDULED")))))))))
    (should (equal osa-test-log '((inside-function-continued) (outer-reset))))
    (should (= (length osa-test-warnings) 1))
    (let ((warning (car osa-test-warnings)))
      (should (string-match-p "target.org" warning))
      (should (string-match-p "ON_DONE" warning))
      (should (string-match-p "osa-test-missing-function" warning))
      (should-not (string-match-p "osa-test-\\(?:reset\\|secondary\\)-missing-function" warning)))))

(defun osa-test-change-target-then-error ()
  (osa-test-change "trimmer-finish" "TODO")
  (error "origin failure"))

(defun osa-test-warning-origin (target-rule)
  (osa-test-with-fixture
    (osa-test-enable osa-test-source)
    (osa-test-enable osa-test-target)
    (osa-test-rule osa-test-source "DONE" '(osa-test-change-target-then-error))
    (when target-rule
      (osa-test-rule osa-test-target "TODO" '(osa-test-note forbidden-pending)))
    (osa-test-change "trimmer-start" "DONE")
    (should-not osa-test-log)
    (should (= (length osa-test-warnings) 1))
    (let ((warning (car osa-test-warnings)))
      (should (string-match-p "Source" warning))
      (should (string-match-p "source.org" warning))
      (should (string-match-p "ON_DONE" warning))
      (should (string-match-p "osa-test-change-target-then-error" warning))
      (should (string-match-p "origin failure" warning))
      (should-not (string-match-p "target.org\\|ON_TODO" warning)))))

(ert-deftest osa-warning-keeps-origin-after-nested-transition-without-rule ()
  (osa-test-warning-origin nil))

(ert-deftest osa-warning-keeps-origin-after-nested-transition-with-rule ()
  (osa-test-warning-origin t))

(ert-deftest osa-load-core-only-dispatches-custom-action ()
  ;; The parent suite imports both modules, so prove isolation in a fresh Emacs.
  (let ((default-directory (file-name-directory (locate-library "org-onstate"))))
    (with-temp-buffer
      (let ((status
             (call-process
              (expand-file-name invocation-name invocation-directory) nil t nil
              "-Q" "--batch" "-L" default-directory "--eval"
              (prin1-to-string
               '(progn
                  (require 'org-onstate)
                  (when (featurep 'org-onstate-actions)
                    (error "Loading core loaded built-in actions"))
                  (defun osa-child-custom-action (value)
                    (org-entry-put nil "CUSTOM_RESULT" value))
                  (let ((org-element-use-cache nil) (org-log-done nil))
                    (with-temp-buffer
                      (insert "* TODO Source\n:PROPERTIES:\n:ON_DONE: (osa-child-custom-action \"ran\")\n:END:\n")
                      (org-mode)
                      (goto-char (point-min))
                      (org-onstate-mode 1)
                      (org-todo "DONE")
                      (unless (equal (org-entry-get nil "CUSTOM_RESULT") "ran")
                        (error "Core did not dispatch the custom action"))))
                  (when (featurep 'org-onstate-actions)
                    (error "Custom dispatch loaded built-in actions"))
                  (princ "CORE-ONLY-OK\n"))))))
        (ert-info ((buffer-string))
          (should (equal status 0))
          (should (string-match-p "CORE-ONLY-OK" (buffer-string))))))))

(ert-deftest osa-load-actions-brings-in-core ()
  (let ((default-directory (file-name-directory (locate-library "org-onstate"))))
    (with-temp-buffer
      (let ((status
             (call-process
              (expand-file-name invocation-name invocation-directory) nil t nil
              "-Q" "--batch" "-L" default-directory "--eval"
              (prin1-to-string
               '(progn
                  (when (featurep 'org-onstate)
                    (error "Core was already loaded before the dependency test"))
                  (require 'org-onstate-actions)
                  (unless (and (featurep 'org-onstate)
                               (featurep 'org-onstate-actions)
                               (fboundp 'org-onstate-mode)
                               (fboundp 'org-onstate-schedule))
                    (error "Actions did not load core and expose the public API"))
                  (princ "ACTIONS-LOAD-OK\n"))))))
        (ert-info ((buffer-string))
          (should (equal status 0))
          (should (string-match-p "ACTIONS-LOAD-OK" (buffer-string))))))))

(provide 'org-onstate-test)
;;; org-onstate-test.el ends here
