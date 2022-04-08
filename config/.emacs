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

(require 'mydefs)

(require 'general)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(backup-by-copying t)
 '(backup-directory\.alist '(("." . "~/.emacs.d/tmp")))
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default))
 '(darkroom-margins 0.2)
 '(debug-on-quit nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(epg-pinentry-mode 'loopback)
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
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(mouse-wheel-follow-mouse 't)
 '(mouse-wheel-progressive-speed 1)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(org-export-backends '(ascii beamer html icalendar latex md odt org))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-link))
 '(org-tags-column 40)
 '(package-selected-packages
   '(org-agenda lsp-treemacs 'use-package lsp-mode company burly neotree dedicated sticky-windows gruber-darker-theme csv-mode graphviz-dot-mode zoom-window ox-hugo org-beautify-theme sublime-themes monokai-theme ivy mu4e-views mu4e flycheck-clj-kondo darkroom org-num w3m clipetty ddg circe lui websocket org-gcal oauth2 default-text-scale org-ehtml ox-twbs ox-minutes ox-epub ox-clip ox-asciidoc ox-pandoc ox-jira ox-slack org-alert org-pdfview org-jira ox-odt highlight-indent-guides multi-term dash-functional ox-confluence htmlize ox-md ox-markdown ob-clojure expand-region powerline writeroom-mode pandoc pandoc-mode groovy-mode kubernetes k8s-mode dockerfile-mode nov markdown-mode jira-markup-mode yaml-mode cider magit ace-window bbdb-vcard bbdb-csv-import bbdb-ext bbdb edit-server quelpa use-package slime))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((epa-encrypt-to "marcus@pemer.io")
     (auto-revert-mode . 1)))
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
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(window-divider-default-places t)
 '(xterm-mouse-mode t)
 '(zoom-window-mode-line-color "DarkGreen"))

;;    (setf epa-encrypt-to "1D151FF890EE620251BC79A4E594D6C2CC9E1BAA")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#080804" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Menlo")))))

(if (daemonp)
   (add-hook 'after-make-frame-functions
             (lambda (frame)
               (with-selected-frame frame
                 (set-cursor-color "#eebbaa"))))
 (set-cursor-color "#eebbaa"))


(provide '.emacs)
;;; .emacs ends here
