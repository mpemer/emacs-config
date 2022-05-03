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

(dolist (pkg (list 'all-the-icons
                   'doom-themes
                   ))
  (my/ensure-package-installed pkg))


(use-package all-the-icons)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-old-hope t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq backup-directory-alist (list (cons "." (concat user-emacs-directory "tmp"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(backup-by-copying t)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(doom-old-hope))
 '(custom-safe-themes
   '("9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" default))
 '(debug-on-quit nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(epg-pinentry-mode 'loopback)
 '(frame-brackground-mode 'dark)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(horizontal-scroll-bar-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(linum-format " %7i ")
 '(menu-bar-mode nil)
 '(mouse-wheel-follow-mouse 't)
 '(mouse-wheel-progressive-speed 1)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(org-caldav-save-directory "~/org/")
 '(package-selected-packages
   '(async emacs-async org-caldav org-gcal org-bullets ox-hugo ox-odt ox-twbs ox-slack ox-minutes ox-jira ox-epub ox-clip ox-asciidoc ox-pandoc org-jira org-ehtml org-alert pandoc nov yasnippet csv-mode yaml-mode kubernetes dockerfile-mode magit flycheck-clj-kondo company flycheck cider which-key dap-mode lsp-ui lsp-mode clojure-mode doom-themes all-the-icons neotree dedicated graphviz-dot-mode clipetty darkroom zoom-window queue oauth2 powerline expand-region multi-term edit-server exec-path-from-shell use-package quelpa slime))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((auto-revert-mode . 1)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-error-top-bottom t)
 '(scroll-margin 2)
 '(scroll-preserve-screen-position nil)
 '(scroll-step 2)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(version-control t)
 '(visible-bell nil)
 '(visual-line-mode t t)
 '(window-divider-default-places t)
 '(xterm-mouse-mode t)
 '(zoom-window-mode-line-color "DarkGreen"))

(if (daemonp)
   (add-hook 'after-make-frame-functions
             (lambda (frame)
               (with-selected-frame frame
                 (set-cursor-color "#eebbaa"))))
 (set-cursor-color "#eebbaa"))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Different fonts between MacOS and GNU/Linux
(set-face-attribute 'default nil :family (if (eq system-type 'darwin) "Menlo" "DejaVu Sans Mono"))
(set-face-attribute 'default nil :height (if (eq system-type 'darwin) 140 120))

;; Some org-mode overrides
;; '(org-document-title ((t (:inherit org-level-1 :foreground "#ee7b29" :box (:line-width 5 :color "#000000") :underline nil :height 2.0))))
;; '(org-level-1 ((t (:inherit outline-1 :extend nil :foreground "#ccbbaa" :slant normal :weight normal :height 1.5 :width normal :foundry "nil" :family "Lucida Grande"))))
;; '(org-level-2 ((t (:inherit default :extend nil :foreground "#cbccd1" :slant normal :weight normal :height 1.25 :width normal :foundry "nil" :family "Lucida Grande"))))
;; '(org-level-3 ((t (:inherit default :extend nil :foreground "#cbccd1"))))

;;(dolist (face '(org-level-1 org-level-2 org-level-3))
;;  (set-face-attribute face nil :box nil))

(provide '.emacs)
;;; .emacs ends here
