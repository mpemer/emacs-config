(setq package-user-dir "~/.emacs.d/elpa"
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "https://melpa.org/packages/")))

(defun ensure-package-installed (package)
  (unless (package-installed-p package) (package-install package)))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(package-initialize)
(unless (file-exists-p package-user-dir) (package-refresh-contents))
(ensure-package-installed 'use-package)

(progn
  (ensure-package-installed 'quelpa)
  (use-package quelpa
    :config (progn
	      (setq quelpa-upgrade-p t
		    quelpa-self-upgrade-p nil))))

(use-package coleslaw
  :config (coleslaw-setup))
;;  :init (quelpa 'coleslaw))



;; Packages that don't have individual configs go here
(let ((package-list
       '(ace-window
 	groovy-mode
	haskell-mode
	edit-server
	wgrep
	pandoc
	pandoc-mode
	puppet-mode      
	copy-as-format
	yaml-mode
	writeroom-mode
	dockerfile-mode
	k8s-mode
	kubernetes
	gruber-darker-theme
	)))
  (dolist (package package-list)
    (progn (ensure-package-installed package)
	   (use-package package))))


;; ace-window
(progn
  (ensure-package-installed 'ace-window)
  (use-package ace-window
    :config (progn
	      (global-set-key (kbd "M-z") 'ace-window))))

;; magit
(progn
  (ensure-package-installed 'magit)
  (use-package magit
    :config
    (progn
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x C-g") 'magit-status))))

;; slime
(progn
  (ensure-package-installed 'slime)
  (use-package slime
    :config
    (setq inferior-lisp-program "sbcl")))

;; cider
(progn
  (ensure-package-installed 'cider)
  (use-package cider
    :config (progn
	      (global-set-key (kbd "<insert>") 'cider-pprint-eval-defun-at-point)
	      (setq cider-show-error-buffer 'only-in-repl)
	      (setq cider-lein-parameters "repl :headless :host localhost")
	      (setq cider-jdk-src-paths '("~/src/clojure"
					  "~/src/openjdk-8"))
	      (setq cider-font-lock-dynamically '(macro core function var))
	      (setq cider-overlays-use-font-lock t)
	      (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
	      (add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
	      (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
	      (add-hook 'cider-mode-hook #'eldoc-mode))))

;; neotree - tree file view
(progn
  (ensure-package-installed 'neotree)
  (use-package neotree
    :config (progn
	      (global-set-key [f12] 'neotree-toggle))))

;; yaml-mode
(progn
  (ensure-package-installed 'yaml-mode)
  (use-package yaml-mode
    :config (progn
	      (add-hook 'yaml-mode-hook
			(lambda ()
			  (define-key yaml-mode-map "\C-m" 'newline-and-indent))))))


;; Auto-complete framework (COMPlete ANYthing)
(progn
  (ensure-package-installed 'company)
  (use-package company
    :config (progn
	      (global-company-mode))))
;;	    (global-set-key (kbd "TAB") #'company-indent-or-complete-common)))



(require 'color)

;; Markdown mode
(progn
  (ensure-package-installed 'markdown-mode)
  (use-package markdown-mode
    :config (progn
	      (add-to-list 'auto-mode-alist '("\\.eml\\'" . markdown-mode)))))


;; EPUB Reader
(progn
  (ensure-package-installed 'nov)
  (use-package nov
    :config (progn
	      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))))




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

;; Navigation
(global-set-key (kbd "M-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-v") 'yank) ; 【Ctrl+v - I compulsively hit this chord for "paste"】
;; Remap the window management keys to something more manageable
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Text scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
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


;; Run emacs server (so we can use emacsclient) if it is not already started
(require 'server)
(unless (server-running-p) (server-start))


(edit-server-start)
