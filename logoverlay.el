(require 'json)

(defun logoverlay--read-file-to-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar logoverlay--logged-overlays ())
(make-variable-buffer-local 'logoverlay--logged-overlays)

(defvar logoverlay--log-contents "")
(defvar logoverlay--console-log-regexp "__LOG__(\\([^)]+\\)).*")
(setq logoverlay--console-log-regexp "__LOG__(\\([^)]+\\)).*")

(defvar logoverlay--logfile "/tmp/log.json")

(defun logoverlay--update-log-contents ()
  (setq logoverlay--log-contents (json-read-file logoverlay--logfile)))

(defun logoverlay--clear-log ()
  (if (file-exists-p logoverlay--logfile)
      (delete-file logoverlay--logfile)))

(defun logoverlay--get-log-entries-by-file-and-id (file id)
  (remove-if-not (lambda (x)
                   (and
                    (equal (cdr (assoc 'file x)) file)
                    (equal (format "%s" (cdr (assoc 'id x)))
                           (format "%s" id))))
                 logoverlay--log-contents))

(defvar logoverlay-buffer-file-name (find-lisp-object-file-name 'update-log-overlays 'defun))

(defun logoverlay--make-log-overlay (bounds)
  (let ((entries (logoverlay--get-log-entries-by-file-and-id (buffer-file-name)
                                                             (cdr (assoc 'id bounds)))))
    (message "entries: %s" entries)
    (mapcar (lambda (log)
              (let ((value (json-encode (cdr (assoc 'value log))))
                    (overlay (make-overlay (cdr (assoc 'start bounds))
                                           (cdr (assoc 'end bounds))
                                           )))
                (push overlay logoverlay--logged-overlays)
                (overlay-put overlay 'face '(underline . green))
                (overlay-put overlay 'help-echo value)
                (overlay-put overlay 'after-string (concat " -> " value))))
            entries)))

(defun logoverlay--find-logs ()
  (let ((logs nil))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward logoverlay--console-log-regexp nil t)
        (push (list (cons 'start (line-beginning-position))
                    (cons 'end (line-end-position))
                    (cons 'id (match-string-no-properties 1)))
              logs)))
    logs))
;; __LOG__(1)


(defun logoverlay--overlay-logs (logs)
  (mapcar 'logoverlay--make-log-overlay logs))

(defun logoverlay--remove-log-overlays ()
  (mapcar 'delete-overlay logoverlay--logged-overlays)
  (setq logoverlay--logged-overlays ()))


(defun logoverlay-clear-log ()
  (interactive)
  (logoverlay--clear-log))

(defun logoverlay-update-log ()
  (interactive)
  (logoverlay--update-log-contents)
  (logoverlay--remove-log-overlays)
  (logoverlay--overlay-logs (logoverlay--find-logs)))

(provide 'logoverlay)
