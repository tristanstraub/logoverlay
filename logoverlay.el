(require 'json)

(defun read-file-to-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar logged-overlays ())
(make-variable-buffer-local 'logged-overlays)

(defvar log-contents "")
(defvar console-log-regexp "__LOG__(\\([^)]+\\)).*")
(setq console-log-regexp "__LOG__(\\([^)]+\\)).*")

(defun read-log-file (file)
   "Read the contents of a file into a temp buffer and then do
 something there."
   (when (file-readable-p file)
     (with-temp-buffer
       (insert-file-contents file)
       (beginning-of-buffer)
       (while (re-search-forward (regexp-quote "log:") nil t)
         ))))

(defun update-log-contents ()
  (setq log-contents (json-read-file "log.json")))

(defun get-log-entries-by-file-and-id (file id)
  (remove-if-not (lambda (x)
                   (and
                    (equal (cdr (assoc 'file x)) file)
                    (equal (cdr (assoc 'id x)) id)))
                 log-contents))

(defun make-log-overlay (bounds)
  (let ((entries (get-log-entries-by-file-and-id (buffer-file-name)
                                         (cdr (assoc 'id bounds)))))
    (mapcar (lambda (log)
              (let ((value (json-encode (cdr (assoc 'value log))))
                    (overlay (make-overlay (cdr (assoc 'start bounds))
                                           (cdr (assoc 'end bounds)))))
                (push overlay logged-overlays)
                (overlay-put overlay 'face '(underline . green))
                (overlay-put overlay 'help-echo value)
                (overlay-put overlay 'after-string (concat " -> " value))))
            entries)))

(defun find-logs ()
  (let ((logs nil))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward console-log-regexp nil t)
        (push (list (cons 'start (line-beginning-position))
                    (cons 'end (line-end-position))
                    (cons 'id (match-string-no-properties 1)))
              logs)))
    logs))
;; __LOG__(1)


(defun overlay-logs (logs)
  (mapcar 'make-log-overlay logs))

(defun remove-log-overlays ()
  (mapcar 'delete-overlay logged-overlays)
  (setq logged-overlays ()))

(defun update-log-overlays ()
  (update-log-contents)
  (remove-log-overlays)
  (overlay-logs (find-logs)))

(provide 'logoverlay)
