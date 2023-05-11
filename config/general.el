;;; Package --- Summary:

;;; Commentary:


;;; Code:

(require 'defs)

(setq package-user-dir (concat user-emacs-directory "elpa")
      package-archives '(("melpa"  . "https://melpa.org/packages/")
			 ("elpa"   . "https://tromey.com/elpa/")
			 ("org"    . "https://orgmode.org/elpa/")
			 ("gnu"    . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("ox-odt" . "https://kjambunathan.github.io/elpa/"))
      package-archive-priorities '(("melpa"  . 20)
				   ("elpa"   . 10)
				   ("org"    . 30)
                                   ("gnu"    . 25)
                                   ("nongnu" . 15)
                                   ("ox-odt" . 10)))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(setq package-check-signature nil)

(package-initialize)

;; Ensure emacs can handle pgp key passowrds using loopback
(setq epa-pinentry-mode 'loopback)

;; Refresh packages every timeout-seconds (probably 24h)
(let* ((ts-file (my/mkfpath package-user-dir ".last-refresh"))
       (last-ts (if (file-exists-p ts-file) (my/read-integer ts-file) 0))
       (timeout-seconds (* 24 60 60))
       (now (time-convert nil 'integer)))
  (when (> (- now last-ts) timeout-seconds)
    (package-refresh-contents) ;; update packages
    (with-temp-file ts-file ;; we did it, so update stored time stamp
      (insert (format "%d\n" now)))))

(my/ensure-package-installed 'quelpa)

(dolist (pkg (list 'use-package))
  (my/ensure-package-installed pkg))

(use-package quelpa
  :config (progn
	          (setq quelpa-upgrade-p t
		              quelpa-self-upgrade-p nil)))


;; general, shared settings divided into separate files
(let ((config-path (my/mkpath user-emacs-directory "config" "general")))
  (when (file-exists-p config-path)
    (add-to-list 'load-path config-path)
    (dolist (file-name (directory-files config-path))
	    (when (or (string-match-p "\.el$" file-name)
		            (string-match-p "\.el.gpg$" file-name))
        (let ((pkg (intern (file-name-base (file-name-base file-name)))))
          (require pkg))))))

;; User-specific settings (files containing secret things are pgp encrypted)
(let ((user-config-path (my/mkpath user-emacs-directory "config" my/user)))
  (when (file-exists-p user-config-path)
    (add-to-list 'load-path user-config-path)
    (dolist (file-name (directory-files user-config-path))
      (if (string-match-p "\.el$" file-name)
          (require (intern (file-name-base (file-name-base file-name))))
	(when (string-match-p "\.el.gpg$" file-name)
	  (load file-name))))))

(provide 'general)
;;; general.el ends here
