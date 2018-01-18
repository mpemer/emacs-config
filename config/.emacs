;; Packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; There are some precompilation warnings that are suppressed
;; only by vaccously declaring some variables.
;; I keep these declarations in defvars.el
(load "~/.emacs.d/config/defvars.el")

;; Initialize package system and declare/install all packages we use
(load "~/.emacs.d/config/packages.el")

;; General settings
(load "~/.emacs.d/config/general.el")

;; Custom functions
(load "~/.emacs.d/config/check-external-modifications.el")
(load "~/.emacs.d/config/narrow-or-widen.el")
(load "~/.emacs.d/config/pretty-print-xml-region.el")

;; User-specific settings (files containing secret things are pgp encrypted)
(let ((config-path (concat "~/.emacs.d/config/" (getenv "USER"))))
  (if (file-exists-p config-path)
      (dolist (file-name (directory-files config-path))
	(if (string-match-p (regexp-quote ".el") file-name)
	    (load (concat config-path "/" file-name))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("89336ca71dae5068c165d932418a368a394848c3b8881b2f96807405d8c6b5b6" "dcf7154867ba67b250fe2c5cdc15a7d170acd9cbe6707cc36d9dd1462282224d" default)))
 '(package-selected-packages
   (quote
    (copy-as-format emacs-slack gruber-darker-theme slime org-sync org-pdfview org-page org-jira org-ac org-bullets org-beautify-theme org-alert org-mime ox-pandoc pandoc-mode pandoc org-plus-contrib edit-server neotree magit groovy-mode markdown-mode cider ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
