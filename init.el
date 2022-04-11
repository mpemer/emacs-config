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

(setq backup-directory-alist (list (cons "." (concat user-emacs-directory "tmp"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(backup-by-copying t)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(doom-old-hope))
 '(custom-safe-themes
   '("850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" default))
 '(debug-on-quit nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(epg-pinentry-mode 'loopback)
 '(exwm-floating-border-color "#282633")
 '(fci-rule-color "#3C3D37")
 '(frame-brackground-mode 'dark)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(horizontal-scroll-bar-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#464258" "#C5A3FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#464258" "#C2FFDF"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#464258" "#656565"))
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(linum-format " %7i ")
 '(mouse-wheel-follow-mouse 't)
 '(mouse-wheel-progressive-speed 1)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(objed-cursor-color "#CC6666")
 '(package-selected-packages
   '(ox-hugo ox-odt ox-twbs ox-slack ox-minutes ox-jira ox-epub ox-clip ox-asciidoc ox-pandoc org-jira org-ehtml org-alert pandoc nov yasnippet csv-mode yaml-mode kubernetes dockerfile-mode magit flycheck-clj-kondo company flycheck cider which-key dap-mode lsp-ui lsp-mode clojure-mode doom-themes all-the-icons neotree dedicated graphviz-dot-mode clipetty darkroom zoom-window queue oauth2 powerline expand-region multi-term edit-server exec-path-from-shell use-package quelpa slime))
 '(pdf-view-midnight-colors (cons "#F8F8F0" "#5a5475"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function 'ignore)
 '(rustic-ansi-faces
   ["#5a5475" "#CC6666" "#C2FFDF" "#FFEA00" "#55b3cc" "#FFB8D1" "#96CBFE" "#F8F8F0"])
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
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#cbccd1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Monospace"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :foreground "#ccbbaa")))))

(set-face-attribute 'default nil :family (if (eq system-type 'darwin) "Menlo" "DejaVu Sans Mono"))
(set-face-attribute 'default nil :height (if (eq system-type 'darwin) 140 120))

(provide '.emacs)
;;; .emacs ends here
