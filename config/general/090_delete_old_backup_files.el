(defun mp/delete-old-backup-files ()
  (interactive)
  (message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file)))))

(run-with-timer 0 86400 'mp/delete-old-backup-files)

