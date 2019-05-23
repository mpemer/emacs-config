;; Packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; Suppress warnings
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; There are some precompilation warnings that are suppressed
;; only by vacuously declaring some variables.
;; I keep these declarations in defvars.el
(load "~/.emacs.d/config/defvars.el")

;; Initialize package system and declare/install all packages we use
(load "~/.emacs.d/config/general.el")

;; User-specific settings (files containing secret things are pgp encrypted)
(let ((config-path (concat "~/.emacs.d/config/" (getenv "USER"))))
  (if (file-exists-p config-path)
      (dolist (file-name (directory-files config-path))
	(if (or (string-match-p "\.el$" file-name)
		(string-match-p "\.el.gpg$" file-name))
	    (load (concat config-path "/" file-name))))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default bold shadow italic underline bold bold-italic bold])
;;  '(ansi-color-names-vector
;;    (vector "#373b41" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
;;  '(beacon-color "#cc6666")
;;  '(column-number-mode t)
;;  '(custom-enabled-themes (quote (gruber-darker)))
;;  '(custom-safe-themes
;;    (quote
;;     ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "89336ca71dae5068c165d932418a368a394848c3b8881b2f96807405d8c6b5b6" "018d40c4ffe70e1863259f0b4614c52bab0cf8e04aeff812b9236fbb184e282e" "3a2a801198c8c3b6cbaa0e2a180176d706c17cc2067abd01898a79e1616a6692" "dd2ef0ab91224977210ee1f1bc5fef4fabff6490d94501df8d2e3661a1536c88" default)))
;;  '(display-time-mode t)
;;  '(fci-rule-color "#373b41")
;;  '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
;;  '(frame-background-mode (quote dark))
;;  '(frame-brackground-mode (quote dark))
;;  '(package-selected-packages
;;    (quote
;;     (ob-tmux ob-http ob-browser ob-async color-theme-sanityinc-tomorrow bbdb-vcard bbdb-csv-import bbdb-ext bbdb jira-markup-mode impatient-mode quelpa os-coleslaw os-md ox-s5 ox-taskjuggler ox-deck ox-confluence org-secretary ord-mime org-depend yaml use-package kubernetes k8s-mode writeroom-mode nov dockerfile-mode puppet-mode yaml-mode wgrep symon slime pandoc-mode pandoc ox-pandoc org-plus-contrib org-pdfview org-mime org-jira org-bullets org-beautify-theme org-alert org-ac oauth2 neotree markdown-mode magit haskell-mode gruber-darker-theme groovy-mode edit-server copy-as-format confluence company cider ace-window)))
;;  '(safe-local-variable-values
;;    (quote
;;     ((epa-encrypt-to "marcus@pemer.io")
;;      (auto-revert-mode . 1))))
;;  '(tool-bar-mode nil)
;;  '(vc-annotate-background nil)
;;  '(vc-annotate-color-map
;;    (quote
;;     ((20 . "#cc6666")
;;      (40 . "#de935f")
;;      (60 . "#f0c674")
;;      (80 . "#b5bd68")
;;      (100 . "#8abeb7")
;;      (120 . "#81a2be")
;;      (140 . "#b294bb")
;;      (160 . "#cc6666")
;;      (180 . "#de935f")
;;      (200 . "#f0c674")
;;      (220 . "#b5bd68")
;;      (240 . "#8abeb7")
;;      (260 . "#81a2be")
;;      (280 . "#b294bb")
;;      (300 . "#cc6666")
;;      (320 . "#de935f")
;;      (340 . "#f0c674")
;;      (360 . "#b5bd68"))))
;;  '(vc-annotate-very-old-color nil))
;; ;; '(send-mail-function (quote mailclient-send-it)))
;;  ;;'(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 158 :width normal)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (org-beautify)))
 '(custom-safe-themes
   (quote
    ("4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default)))
 '(package-selected-packages
   (quote
    (ob-clojure expand-region powerline writeroom-mode pandoc pandoc-mode groovy-mode kubernetes k8s-mode dockerfile-mode nov markdown-mode jira-markup-mode yaml-mode cider magit ace-window bbdb-vcard bbdb-csv-import bbdb-ext bbdb edit-server gruber-darker-theme quelpa use-package slime)))
 '(safe-local-variable-values (quote ((auto-revert-mode . 1)))))
