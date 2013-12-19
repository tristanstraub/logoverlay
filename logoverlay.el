(require 'json)

(defun logoverlay--read-file-to-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar logoverlay--logged-overlays ())
(make-variable-buffer-local 'logoverlay--logged-overlays)

(defvar logoverlay--showing-overlays nil)
(make-variable-buffer-local 'logoverlay--showing-overlays)

(defvar logoverlay--log-contents "")
(defvar logoverlay--console-log-regexp "")
(setq logoverlay--console-log-regexp "__LOG__(\\([^,]+\\).*")

(defvar logoverlay--buffer-id-regexp "")
(setq logoverlay--buffer-id-regexp "\\(LOGID=[A-Za-z0-9]*\\)")

(defvar logoverlay--logfile "/tmp/log.json")

(defun logoverlay--update-log-contents ()
  (if (file-exists-p logoverlay--logfile)
      (setq logoverlay--log-contents (json-read-file logoverlay--logfile))
    (setq logoverlay--log-contents nil)))

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

(defun logoverlay--get-buffer-id ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward logoverlay--buffer-id-regexp nil t)
    (let ((id (match-string-no-properties 1)))
      (or id (buffer-file-name)))))

(defun logoverlay--make-log-overlay (bounds)
  (let ((entries (logoverlay--get-log-entries-by-file-and-id (logoverlay--get-buffer-id)
                                                             (cdr (assoc 'id bounds)))))
    (mapcar (lambda (log)
              (let ((value (json-encode (cdr (assoc 'value log))))
                    (overlay (make-overlay (cdr (assoc 'start bounds))
                                           (cdr (assoc 'end bounds))
                                           )))
                (push overlay logoverlay--logged-overlays)
                (overlay-put overlay 'evaporate t)
                (overlay-put overlay 'face '(underline . green))
                (overlay-put overlay 'help-echo value)
                (overlay-put overlay 'after-string (concat "\n -> " value))))
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

(defun logoverlay--overlay-logs (logs)
  (mapcar 'logoverlay--make-log-overlay logs)
  (setq logoverlay--showing-overlays t)

  (logoverlay--start-timer))

(defun logoverlay--start-timer ()
  (if (not logoverlay--monitor-timer)
      (setq logoverlay--monitor-timer 
            (logoverlay--install-monitor "/tmp/log.json" 5 'logoverlay--log-changed))))

(defun logoverlay--remove-log-overlays ()
  (mapcar 'delete-overlay logoverlay--logged-overlays)
  (setq logoverlay--logged-overlays ())
  (setq logoverlay--showing-overlays nil))


(defun logoverlay-clear-log ()
  (interactive)
  (logoverlay--clear-log))

(defun logoverlay-remove-overlays ()
  (interactive)
  (logoverlay--remove-log-overlays))

(defun logoverlay-update-overlays ()
  (interactive)
  (logoverlay--update-log-contents)
  (logoverlay--remove-log-overlays)
  (logoverlay--overlay-logs (logoverlay--find-logs)))

(defun logoverlay-toggle-overlays ()
  (interactive)
  (if logoverlay--showing-overlays
      (logoverlay-remove-overlays)
    (logoverlay-update-overlays)
    ))

(defun logoverlay--refresh-overlays ()
  (if logoverlay--showing-overlays
      (progn
        (logoverlay-toggle-overlays)
        (logoverlay-toggle-overlays))))

(defvar logoverlay--monitor-attributes nil
  "Cached file attributes to be monitored.")

(defun logoverlay--install-monitor (file secs fn)
  (run-with-timer
   0 secs
   (lambda (f p fn)
     (let ((att (file-attributes f)))
       (unless (or (null logoverlay--monitor-attributes) (equalp logoverlay--monitor-attributes att))
         (funcall fn))
       (setq logoverlay--monitor-attributes att)))
   file secs fn))

(defun logoverlay--log-changed ()
  (logoverlay--refresh-overlays)
  )

(defvar logoverlay--monitor-timer nil
  "Check if /tmp is changed every 5s.")

(provide 'logoverlay)


