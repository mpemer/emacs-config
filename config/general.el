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

(add-to-list 'load-path "~/.emacs.d/config/")

(let ((config-path "~/.emacs.d/config/general"))
  (if (file-exists-p config-path)
      (dolist (file-name (directory-files config-path))
	(if (or (string-match-p "\.el$" file-name)
		(string-match-p "\.el.gpg$" file-name))
	    (load (concat config-path "/" file-name))))))

(require 'color)

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
;;(menu-bar-mode -1)
(delete-selection-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; I like to see what time it is also when in full screen mode and OS menu bar is hidden
(display-time)

