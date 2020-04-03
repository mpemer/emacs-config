;; Packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; Suppress warnings
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; Set default font size
(set-face-attribute 'default nil :height 140)

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

 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:foreground "#e4e4ef" :background "#181818")))))
;;  '(term-color-black ((t (:foreground "#313131" :background "#141414"))))
;;  '(term-color-blue ((t (:foreground "#7587A6" :background "#5d6c84"))))
;;  '(term-color-cyan ((t (:foreground "#5d8084" :background "#41595c"))))
;;  '(term-color-green ((t (:foreground "#8F9D6A" :background "#646d4a"))))
;;  '(term-color-magenta ((t (:foreground "#EE799F" :background "#EF2929"))))
;;  '(term-color-red ((t (:foreground "#CF6A4C" :background "#a3472c"))))
;;  '(term-color-white ((t (:foreground "#CACACA" :background "#5F5A60"))))
;;  '(term-color-yellow ((t (:foreground "#efa510" :background "#CDA869"))))
;;  '(term-default-bg-color ((t (:inherit term-color-black))))
;;  '(term-default-fg-color ((t (:inherit term-color-white)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (org-beautify tangotango)))
 '(custom-safe-themes
   (quote
    ("713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))
 '(display-time-mode t)
 ;;'(frame-brackground-mode (quote dark))
 '(frame-background-mode (quote dark))
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed 1)
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt org)))
 '(package-selected-packages
   (quote
    (clipetty clippety ddg circe lui websocket org-gcal oauth2 default-text-scale org-ehtml ox-twbs ox-minutes ox-epub ox-clip ox-asciidoc ox-pandoc ox-jira ox-slack org-alert org-pdfview org-jira org-beautify-theme color-theme-modern tangotango-theme leuven-theme zenburn-theme abyss-theme ox-odt highlight-indent-guides multi-term dash-functional ox-confluence htmlize ox-md ox-markdown ob-clojure expand-region powerline writeroom-mode pandoc pandoc-mode groovy-mode kubernetes k8s-mode dockerfile-mode nov markdown-mode jira-markup-mode yaml-mode cider magit ace-window bbdb-vcard bbdb-csv-import bbdb-ext bbdb edit-server gruber-darker-theme quelpa use-package slime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((epa-encrypt-to "marcus@pemer.io")
     (auto-revert-mode . 1))))
 '(show-paren-mode t)
 '(sql-database "atgprd")
 '(sql-oracle-login-params (quote (user password database)))
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
