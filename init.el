;;; Package --- Summary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; Commentary:

;;; Code:

;; Keep custom config under config directory (a git repo).
;; Entry poinqt to config repo is general.el
(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'defs)
(require 'general)

(dolist (pkg '(all-the-icons
               gruber-darker-theme))
  (my/ensure-package-installed pkg))


(use-package all-the-icons)
(use-package gruber-darker-theme)

(setq backup-directory-alist (list (cons "." (concat user-emacs-directory "tmp"))))

;; Specify the file to save custom settings
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))

;; Load the custom settings from the file, if it exists
(when (file-exists-p custom-file)
  (load custom-file))

(provide '.init)
;;; init.el ends here
