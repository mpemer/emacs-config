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
 '(custom-enabled-themes (quote (lagavulin)))
 '(custom-safe-themes
   (quote
    ("018d40c4ffe70e1863259f0b4614c52bab0cf8e04aeff812b9236fbb184e282e" "3a2a801198c8c3b6cbaa0e2a180176d706c17cc2067abd01898a79e1616a6692" "dd2ef0ab91224977210ee1f1bc5fef4fabff6490d94501df8d2e3661a1536c88" default)))
 '(frame-brackground-mode (quote dark)))
;; '(send-mail-function (quote mailclient-send-it)))
 ;;'(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
