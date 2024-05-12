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
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-program "~/org/bin/android_firefox.sh")
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(gruber-darker))
 '(debug-on-quit nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(frame-brackground-mode 'dark)
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
 '(org-caldav-debug-level 1)
 '(org-caldav-oauth2-client-secret "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")
 '(org-caldav-resume-aborted 'never)
 '(org-caldav-sync-todo t)
 '(org-caldav-url 'google)
 '(org-icalendar-use-scheduled
   '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start))
 '(org-icalendar-with-timestamps t)
 '(org-startup-folded t)
 '(package-selected-packages
   '(chatgpt ox-coleslaw org-contrib ox-extra json-mode fzf gruber-darker-theme 'gruber-darker-theme async emacs-async org-caldav org-gcal org-bullets ox-hugo ox-odt ox-twbs ox-slack ox-minutes ox-epub ox-clip ox-asciidoc ox-pandoc org-ehtml org-alert pandoc nov yasnippet csv-mode yaml-mode kubernetes dockerfile-mode magit flycheck-clj-kondo company flycheck cider which-key dap-mode lsp-ui lsp-mode clojure-mode doom-themes all-the-icons neotree dedicated graphviz-dot-mode clipetty darkroom zoom-window queue oauth2 powerline expand-region multi-term edit-server exec-path-from-shell use-package quelpa))
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local)
     (encoding . utf-8)
     (eval put 'test-js-eval 'common-lisp-indent-function 1)
     (eval put 'test-ps-js 'common-lisp-indent-function 1)
     (auto-revert-mode . 1)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-error-top-bottom t)
 '(scroll-margin 2)
 '(scroll-preserve-screen-position nil)
 '(scroll-step 2)
 '(size-indication-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(version-control t)
 '(visible-bell nil)
 '(visual-line-mode t t)
 '(warning-suppress-types '((comp)))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 1 :width normal :foundry "default" :family "default")))))

(provide '.emacs)
;;; .emacs ends here
