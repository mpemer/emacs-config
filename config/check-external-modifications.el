;; Alert when files have been externally modified
(defun mp/check-external-modifications ()
  (if (verify-visited-file-modtime (current-buffer))
      (setq header-line-format nil)
    ;;(if (buffer-modified-p (current-buffer))
	(setq header-line-format (format "*** WARNING [%s] WARNING ***"
					 (propertize "This file has been changed externally" 'face '(:foreground "#f92672")))))) ;;)
(run-with-timer 0 2 'mp/check-external-modifications)
;;
