;;; org-onstate-actions.el --- Built-in actions for org-onstate -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.5"))
;; Keywords: outlines

;;; Commentary:

;; Optional built-in actions for `org-onstate'.

;;; Code:

(require 'org-onstate)
(require 'org-id)

(defun org-onstate--schedule-options (options)
  "Validate schedule helper OPTIONS and return the delay in seconds."
  (let ((remaining options)
        seen)
    (while remaining
      (let ((key (car remaining)))
        (unless (memq key '(:id :after :state))
          (error "Unknown org-onstate-schedule option %S" key))
        (when (memq key seen)
          (error "Duplicate org-onstate-schedule option %S" key))
        (setq remaining (cdr remaining))
        (unless remaining
          (error "Missing value for org-onstate-schedule option %S" key))
        (setq seen (cons key seen))
        (setq remaining (cdr remaining))))
    (unless (memq :id seen)
      (error "org-onstate-schedule requires :id"))
    (unless (memq :after seen)
      (error "org-onstate-schedule requires :after"))
    (let ((id (plist-get options :id))
          (after (plist-get options :after))
          (state (plist-get options :state)))
      (unless (and (stringp id) (> (length id) 0))
        (error ":id must be a nonempty string"))
      (let ((seconds (org-onstate--delay-seconds after)))
        (when (and (memq :state seen) (not (stringp state)))
          (error ":state must be a target TODO keyword string"))
        seconds))))

(defun org-onstate--delay-seconds (after)
  "Validate AFTER and convert it to elapsed seconds."
  (unless (and (stringp after)
               (string-match "\\`\\([1-9][0-9]*\\)\\([mhd]\\)\\'" after))
    (error ":after must be a positive integer followed by m, h, or d"))
  (let ((amount (string-to-number (match-string 1 after)))
        (unit (match-string 2 after)))
    (* amount
       (cond
        ((equal unit "m") 60)
        ((equal unit "h") 3600)
        (t 86400)))))

;;;###autoload
(defun org-onstate-schedule (&rest options)
  "Schedule the Org heading selected by OPTIONS after an elapsed delay.

OPTIONS accepts required `:id' and `:after' values and optional `:state'.
`:after' is a positive integer followed by m, h, or d.  The target is
scheduled before its TODO state is changed."
  (let* ((seconds (org-onstate--schedule-options options))
         (id (plist-get options :id))
         (state (plist-get options :state))
         (base-time (if org-onstate--current-event
                        (plist-get org-onstate--current-event :time)
                      (current-time)))
         (schedule-time (time-add base-time (seconds-to-time seconds)))
         (marker (org-id-find id 'marker)))
    (unless (and (markerp marker)
                 (marker-position marker)
                 (buffer-live-p (marker-buffer marker)))
      (when (markerp marker)
        (set-marker marker nil))
      (error "No live Org heading found for ID %S" id))
    (unwind-protect
        (save-current-buffer
          (set-buffer (marker-buffer marker))
          (unless (derived-mode-p 'org-mode)
            (error "ID %S does not resolve to an Org buffer" id))
          (save-restriction
            (widen)
            (save-excursion
              (goto-char marker)
              (org-back-to-heading t)
              (unless (org-at-heading-p)
                (error "ID %S does not resolve to an Org heading" id))
              (barf-if-buffer-read-only)
              (when (and state
                         (not (member state org-todo-keywords-1)))
                (error "%S is not an exact TODO keyword in the target buffer"
                       state))
              (when (org-get-repeat)
                (error "Target ID %S has a repeater; only one-off targets are supported"
                       id))
              (let ((old-state (org-get-todo-state))
                    (timestamp (format-time-string "%Y-%m-%d %a %H:%M"
                                                   schedule-time)))
                (org-schedule nil timestamp)
                (when (and state (not (equal state old-state)))
                  (org-todo state))))))
      (set-marker marker nil))))

(provide 'org-onstate-actions)

;;; org-onstate-actions.el ends here
