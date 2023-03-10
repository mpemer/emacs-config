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
                   ;;'doom-themes
                   ;;))
  (my/ensure-package-installed pkg))


(use-package all-the-icons)
(use-package gruber-darker-theme)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-old-hope t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(setq backup-directory-alist (list (cons "." (concat user-emacs-directory "tmp"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1c1408" "#ff4e00" "#7cb518" "#ffbf00" "#0075c4" "#d72638" "#898989" "#5b8512"])
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(backup-by-copying t)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "4699e3a86b1863bbc695236036158d175a81f0f3ea504e2b7c71f8f7025e19e3" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "846b3dc12d774794861d81d7d2dcdb9645f82423565bfb4dad01204fa322dbd5" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" default))
 '(debug-on-quit nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(epg-pinentry-mode 'loopback)
 '(exwm-floating-border-color "#43493f")
 '(fci-rule-color "#95836f")
 '(frame-brackground-mode 'dark)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors ((("#252409") . 0) (("#261f14") . 20)))
 '(horizontal-scroll-bar-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#ffbf00"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#7cb518"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#707a6a"))
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(linum-format " %7i ")
 '(menu-bar-mode nil)
 '(mouse-wheel-follow-mouse 't)
 '(mouse-wheel-progressive-speed 1)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(objed-cursor-color "#ff4e00")
 '(org-caldav-debug-level 1 t)
 '(org-caldav-oauth2-client-secret "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" t)
 '(org-caldav-resume-aborted 'never)
 '(org-caldav-save-directory "~/org/" t)
 '(org-caldav-sync-todo t t)
 '(org-caldav-url 'google t)
 '(org-icalendar-use-scheduled
   '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start))
 '(org-icalendar-with-timestamps t)
 '(package-selected-packages
   '(sly-named-readtables sly-contribs org-contrib "ox-extra" ox-extra json-mode fzf gruber-darker-theme 'gruber-darker-theme async emacs-async org-caldav org-gcal org-bullets ox-hugo ox-odt ox-twbs ox-slack ox-minutes ox-jira ox-epub ox-clip ox-asciidoc ox-pandoc org-jira org-ehtml org-alert pandoc nov yasnippet csv-mode yaml-mode kubernetes dockerfile-mode magit flycheck-clj-kondo company flycheck cider which-key dap-mode lsp-ui lsp-mode clojure-mode doom-themes all-the-icons neotree dedicated graphviz-dot-mode clipetty darkroom zoom-window queue oauth2 powerline expand-region multi-term edit-server exec-path-from-shell use-package quelpa))
 '(pdf-view-midnight-colors (cons "#5b8512" "#1c1408"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function 'ignore)
 '(rustic-ansi-faces
   ["#1c1408" "#ff4e00" "#7cb518" "#ffbf00" "#0075c4" "#d72638" "#898989" "#5b8512"])
 '(safe-local-variable-values
   '((encoding . utf-8)
     (eval put 'test-js-eval 'common-lisp-indent-function 1)
     (eval put 'test-ps-js 'common-lisp-indent-function 1)
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
 '(vc-annotate-background "#1c1408")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7cb518")
    (cons 40 "#a7b810")
    (cons 60 "#d3bb08")
    (cons 80 "#ffbf00")
    (cons 100 "#ffa400")
    (cons 120 "#ff8a00")
    (cons 140 "#ff7000")
    (cons 160 "#f15712")
    (cons 180 "#e43e25")
    (cons 200 "#d72638")
    (cons 220 "#e43325")
    (cons 240 "#f14012")
    (cons 260 "#ff4e00")
    (cons 280 "#db591a")
    (cons 300 "#b76435")
    (cons 320 "#936f4f")
    (cons 340 "#95836f")
    (cons 360 "#95836f")))
 '(vc-annotate-very-old-color nil)
 '(version-control t)
 '(visible-bell nil)
 '(visual-line-mode t t)
 '(warning-suppress-types
   '((comp)))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#080604" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 141 :width normal :foundry "nil" :family "Menlo")))))

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
