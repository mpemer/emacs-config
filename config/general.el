(setq package-user-dir "~/.emacs.d/elpa"
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-archives '(
			 ("elpa" . "https://tromey.com/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ("ox-odt" . "https://kjambunathan.github.io/elpa/")
			 ))

(defun ensure-package-installed (package)
  (unless (package-installed-p package) (package-install package)))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(setq package-check-signature nil)
(package-initialize)
(unless (file-exists-p package-user-dir) (package-refresh-contents))
(ensure-package-installed 'use-package)

(progn
  (ensure-package-installed 'oauth2)
  (use-package oauth2))

;;(progn
;;  (ensure-package-installed 'color-theme-modern)
;;  (use-package color-theme-modern))

(progn
  (ensure-package-installed 'default-text-scale)
  (use-package default-text-scale))

(progn
  (ensure-package-installed 'default-text-scale)
  (use-package default-text-scale))

(progn
  (ensure-package-installed 'quelpa)
  (use-package quelpa
    :config (progn
	      (setq quelpa-upgrade-p t
		    quelpa-self-upgrade-p nil))))

(ensure-package-installed 'queue)


(progn
  (ensure-package-installed 'highlight-indent-guides)
  (use-package highlight-indent-guides
    :config (progn
	      (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
	      (setq highlight-indent-guides-auto-odd-face-perc 5)
	      (setq highlight-indent-guides-auto-even-face-perc 5)
	      (setq highlight-indent-guides-auto-character-face-perc 50)
;;	      (setq highlight-indent-guides-method 'fill))))
            (setq highlight-indent-guides-method 'character))))
;;	      (setq highlight-indent-guides-method 'column))))

;; Send emacs kill ring to remote clipboard
(progn
  (ensure-package-installed 'clipetty)
  (use-package clipetty
    :config (global-clipetty-mode)))

(add-to-list 'load-path "~/.emacs.d/config/")

(let ((config-path "~/.emacs.d/config/general"))
  (if (file-exists-p config-path)
      (dolist (file-name (directory-files config-path))
	(if (or (string-match-p "\.el$" file-name)
		(string-match-p "\.el.gpg$" file-name))
	    (load (concat config-path "/" file-name))))))

(require 'color)

;; I like to copy stuff from web pages and make org mode documents out of them.
(defun html-to-org-region (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "pandoc -f html -t markdown_github-raw_html | pandoc -f markdown -t org" (current-buffer) t))
;;  (comment-region (mark) (point)))
(global-set-key (kbd "C-c C-i C-o") 'html-to-org-region)

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

(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/config/abbrev_defs")
(setq save-abbrevs t)

;; Increase minibuffer font size
(dolist
    (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*" "*Quail Completions*"))
  (when (get-buffer buf)
    (with-current-buffer buf
      (setq-local face-remapping-alist '((default (:height 1.2)))))))


(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; I like to see what time it is also when in full screen mode and OS menu bar is hidden
(display-time)

(set-cursor-color "#eebbaa")

