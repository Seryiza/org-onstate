;;; org-onstate.el --- Run heading actions on TODO state changes -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.5"))
;; Keywords: outlines

;;; Commentary:

;; `org-onstate-mode' runs ordinary Elisp functions named in a local
;; ON_<STATE> heading property.  See README.md for setup, syntax, and the
;; security implications of executing properties from an Org file.

;;; Code:

(require 'org)

;; Org dynamically binds these variables while running its state-change hook.
(defvar org-state)
(defvar org-last-state)

(defgroup org-onstate nil
  "Run functions when Org headings enter TODO states."
  :group 'org)

(defcustom org-onstate-max-events 100
  "Maximum number of events processed in one synchronous dispatch chain.
The value must be a positive integer.  This bound terminates action cycles."
  :type 'integer
  :group 'org-onstate)

(defvar org-onstate--dispatching nil)
(defvar org-onstate--event-queue nil)
(defvar org-onstate--current-event nil)
(defvar org-onstate--error-context nil)
(defvar org-onstate--failure nil)

(defun org-onstate--validate-data (value ancestors)
  "Reject improper or circular list data in VALUE.
ANCESTORS contains containers on the current traversal path."
  (cond
   ((consp value)
    (when (memq value ancestors)
      (error "Circular lists are not allowed"))
    (unless (proper-list-p value)
      (error "Improper or circular lists are not allowed"))
    (let ((parents (cons value ancestors)))
      (dolist (element value)
        (org-onstate--validate-data element parents))))
   ((vectorp value)
    (if (memq value ancestors)
        (error "Circular data is not allowed")
      (let ((index 0)
            (parents (cons value ancestors)))
        (while (< index (length value))
          (org-onstate--validate-data (aref value index) parents)
          (setq index (1+ index))))))))

(defun org-onstate--validate-call (call property)
  "Validate one CALL read from PROPERTY."
  (unless (and (consp call) (symbolp (car call)))
    (error "%s must contain function calls beginning with a symbol" property))
  (let ((function (car call)))
    (cond
     ((special-form-p function)
      (error "%s names special form %S, not an ordinary function"
             property function))
     ((macrop function)
      (error "%s names macro %S, not an ordinary function"
             property function))
     ((not (functionp function))
      (error "%s names unknown or non-callable function %S"
             property function)))))

(defun org-onstate--parse (text property)
  "Parse and validate action TEXT from PROPERTY.
Return a list of calls, or nil for an empty action value."
  (if (or (null text)
          (string-match-p "\\`[ \t\r\n]*\\'" text))
      nil
    (let* ((read-circle t)
           (read-symbol-shorthands nil)
           (result (read-from-string text))
           (form (car result))
           (end (cdr result))
           calls)
      (unless (string-match-p "\\`[ \t\r\n]*\\'" (substring text end))
        (error "%s has trailing content after its action expression" property))
      (if (null form)
          nil
        (org-onstate--validate-data form nil)
        (unless (consp form)
          (error "%s must be a function call or a list of function calls"
                 property))
        (setq calls (if (symbolp (car form)) (list form) form))
        (dolist (call calls)
          (org-onstate--validate-call call property))
        calls))))

(defun org-onstate--context (event &optional action index)
  "Build diagnostic context from EVENT, ACTION, and INDEX."
  (if action
      (append event (list :action action :index index))
    event))

(defun org-onstate--context-description (context)
  "Format CONTEXT for a dispatch warning."
  (if (null context)
      "while observing an Org TODO transition"
    (format "at heading %S in %s, transition %S -> %S, property %s%s"
            (plist-get context :heading)
            (plist-get context :file)
            (plist-get context :old)
            (plist-get context :new)
            (or (plist-get context :property) "<none>")
            (if (plist-get context :action)
                (format ", action %d %S"
                        (plist-get context :index)
                        (plist-get context :action))
              ""))))

(defun org-onstate--warn (error-data)
  "Warn about ERROR-DATA using the current dispatch context."
  (display-warning
   'org-onstate
   (format "Stopped action dispatch %s: %s. Pending actions were discarded; completed effects were not rolled back"
           (org-onstate--context-description
            org-onstate--error-context)
           (error-message-string error-data))
   :warning))

(defun org-onstate--release-event (event)
  "Release the source markers held by EVENT."
  (dolist (marker (list (plist-get event :start-marker)
                        (plist-get event :end-marker)))
    (when (markerp marker)
      (set-marker marker nil))))

(defun org-onstate--clear-events ()
  "Release all event markers and clear pending dispatch work."
  (when org-onstate--current-event
    (org-onstate--release-event org-onstate--current-event))
  (dolist (event org-onstate--event-queue)
    (org-onstate--release-event event))
  (setq org-onstate--current-event nil)
  (setq org-onstate--event-queue nil))

(defun org-onstate--enqueue-observed-event ()
  "Snapshot and enqueue the TODO state change at point, when selected."
  (when (and org-state
             (not (equal org-state org-last-state))
             (member org-state org-todo-keywords-1))
    (let ((event-time (current-time)))
      (save-excursion
        (org-back-to-heading t)
        (let* ((property (concat "ON_" org-state))
               (text (org-entry-get (point) property nil))
               (event (list :heading (org-get-heading t t t t)
                            :file (or buffer-file-name (buffer-name))
                            :old org-last-state
                            :new org-state
                            :property property))
               actions
               start-marker
               end-marker)
          (setq org-onstate--error-context event)
          (setq actions (org-onstate--parse text property))
          (when actions
            ;; Insertions at the beginning stay before START-MARKER, while
            ;; insertions at the end stay after END-MARKER.  Deleting and
            ;; replacing the original heading therefore collapses or inverts
            ;; this span instead of silently retargeting it.
            (setq start-marker (copy-marker (line-beginning-position) t))
            (setq end-marker (copy-marker (line-end-position)))
            (setq event
                  (append event
                          (list :start-marker start-marker
                                :end-marker end-marker
                                :time event-time
                                :actions actions)))
            (setq org-onstate--event-queue
                  (nconc org-onstate--event-queue (list event)))))))))

(defun org-onstate--signal-failure ()
  "Signal the first nested observation failure, when one is latched."
  (when org-onstate--failure
    (setq org-onstate--error-context
          (plist-get org-onstate--failure :context))
    (let ((error-data (plist-get org-onstate--failure :error)))
      (signal (car error-data) (cdr error-data)))))

(defun org-onstate--run-action (event action)
  "Run ACTION at the source heading recorded by EVENT."
  (let ((start-marker (plist-get event :start-marker))
        (end-marker (plist-get event :end-marker)))
    (unless (and (markerp start-marker)
                 (markerp end-marker)
                 (marker-position start-marker)
                 (marker-position end-marker)
                 (buffer-live-p (marker-buffer start-marker))
                 (eq (marker-buffer start-marker)
                     (marker-buffer end-marker))
                 (< (marker-position start-marker)
                    (marker-position end-marker)))
      (error "The source heading is no longer live"))
    (with-current-buffer (marker-buffer start-marker)
      (save-restriction
        (save-excursion
          (widen)
          (goto-char start-marker)
          (unless (and (= (point) (line-beginning-position))
                       (org-at-heading-p)
                       (<= (marker-position end-marker)
                           (line-end-position)))
            (error "The original source heading no longer spans its markers"))
          (org-back-to-heading t)
          (let ((org-last-state (plist-get event :old))
                (org-state (plist-get event :new)))
            (apply (car action) (cdr action))))))))

(defun org-onstate--drain-events ()
  "Run queued events in FIFO order."
  (unless (and (integerp org-onstate-max-events)
               (> org-onstate-max-events 0))
    (error "org-onstate-max-events must be a positive integer"))
  (let ((processed-events 0))
    (while org-onstate--event-queue
      (org-onstate--signal-failure)
      (when (>= processed-events org-onstate-max-events)
        (setq org-onstate--error-context
              (org-onstate--context
               (car org-onstate--event-queue)))
        (error "Event limit %d exhausted; the actions may contain a cycle"
               org-onstate-max-events))
      (setq org-onstate--current-event
            (pop org-onstate--event-queue))
      (setq processed-events (1+ processed-events))
      (let ((actions (plist-get org-onstate--current-event :actions))
            (index 1))
        (dolist (action actions)
          (org-onstate--signal-failure)
          (setq org-onstate--error-context
                (org-onstate--context
                 org-onstate--current-event action index))
          (condition-case error-data
              (org-onstate--run-action
               org-onstate--current-event action)
            (error
             (if org-onstate--failure
                 (org-onstate--signal-failure)
               (signal (car error-data) (cdr error-data)))))
          (org-onstate--signal-failure)
          (setq index (1+ index))))
      (org-onstate--release-event org-onstate--current-event)
      (setq org-onstate--current-event nil))))

(defun org-onstate--after-todo-state-change ()
  "Observe an Org TODO state hook and dispatch selected actions."
  (if org-onstate--dispatching
      (unless org-onstate--failure
        (let ((org-onstate--error-context org-onstate--error-context))
          (condition-case error-data
              (org-onstate--enqueue-observed-event)
            (error
             (setq org-onstate--failure
                   (list :error error-data
                         :context org-onstate--error-context))))))
    (let ((org-onstate--dispatching t)
          (org-onstate--event-queue nil)
          (org-onstate--current-event nil)
          (org-onstate--error-context nil)
          (org-onstate--failure nil))
      (unwind-protect
          (condition-case error-data
              (progn
                (org-onstate--enqueue-observed-event)
                (org-onstate--drain-events))
            (error
             (org-onstate--clear-events)
             (org-onstate--warn error-data)))
        (org-onstate--clear-events)))))

;;;###autoload
(define-minor-mode org-onstate-mode
  "Run local ON_<STATE> properties after real Org TODO state changes.

This buffer-local mode must be enabled explicitly in each Org buffer whose
properties should run."
  :lighter " OnState"
  (if org-onstate-mode
      (if (not (derived-mode-p 'org-mode))
          (progn
            (setq org-onstate-mode nil)
            (user-error "org-onstate-mode is valid only in Org buffers"))
        (add-hook 'org-after-todo-state-change-hook
                  #'org-onstate--after-todo-state-change nil t))
    (remove-hook 'org-after-todo-state-change-hook
                 #'org-onstate--after-todo-state-change t)))

(provide 'org-onstate)

;;; org-onstate.el ends here
