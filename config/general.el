;; Navigation
(global-set-key (kbd "M-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-q") 'ace-window)
(global-set-key (kbd "C-v") 'yank) ; 【Ctrl+v - I compulsively hit this chord for "paste"】
;; Remap the window management keys to something more manageable
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)



(global-set-key (kbd "C-<return>") 'magit-stage-all-and-commit)

;; Email
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "marcus@pemer.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-gnutls-program "/usr/local/bin/gnutls-cli"
      starttls-extra-arguments nil
      starttls-use-gnutls t
      mail-self-blind nil
      compose-mail-user-agent-warnings nil
      mail-default-headers "From: Marcus Pemer <marcus@pemer.io>")

(defun my-message-add-bcc ()
  (message-add-header "BCC: marcus@pemer.io"))
(add-hook 'message-send-hook 'my-message-add-bcc)

;; MaGIT
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Cider
(setq cider-show-error-buffer 'only-in-repl)
(global-set-key (kbd "<insert>") 'cider-pprint-eval-defun-at-point)
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
(setq cider-lein-parameters "repl :headless :host localhost")


;; Org
(setq org-export-with-toc nil)

;;(add-hook 'message-mode-hook
;;          (lambda ()
;;            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
;;
;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

;; NeoTree
(global-set-key [f12] 'neotree-toggle)

;; Text scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Enable reopening of recent files via C-x C-r
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; General
(setq visible-bell nil
      ring-bell-function 'ignore
      inhibit-startup-message t
      indent-tabs-mode nil
      tab-width 2
      column-number-mode t
      visual-line-mode t
      display-time-day-and-date t
      display-time-24hr-format t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/tmp"))    ; don't litter my fs tree
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      create-lockfiles nil
      scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position nil
      scroll-error-top-bottom t
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      mouse-wheel-progressive-speed 1 ;don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;scroll window under mouse
      scroll-step 2) ;keyboard scroll one line at a time
;;      epg-gpg-program "gpg")

(setq epa-pinentry-mode 'loopback)
;(pinentry-start)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; I like to see what time it is also when in full screen mode and OS menu bar is hidden
(display-time)


(setf confluence-url "https://iteego.jira.com/wiki/rpc/xmlrpc")
(global-set-key (kbd "C-x wf") 'confluence-get-page)

;; ;;
;; ;; Auto-complete

(global-company-mode)
;;(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

(require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
;;

(add-to-list 'auto-mode-alist '("\\.eml\\'" . markdown-mode))

(setq cider-jdk-src-paths '("~/src/clojure"
                            "~/src/openjdk-8"))
(setq cider-font-lock-dynamically '(macro core function var))
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-overlays-use-font-lock t)

;; Run emacs server (so we can use emacsclient) if it is not already started
(require 'server)
(unless (server-running-p) (server-start))


(edit-server-start)
