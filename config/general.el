;; Navigation
(global-set-key (kbd "M-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-q") 'ace-window)

;; MaGIT
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Cider
(setq cider-show-error-buffer 'only-in-repl)
(global-set-key (kbd "<insert>") 'cider-pprint-eval-defun-at-point)
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)

;; Org
(setq org-export-with-toc nil)

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
      scroll-step 2 ;keyboard scroll one line at a time
      epg-gpg-program "gpg2")

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
