;;; Package --- Summary:

;;; Commentary:


;;; Code:

(require 'mydefs)

(setq package-user-dir (concat user-emacs-directory "elpa")
      package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://tromey.com/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("ox-odt" . "https://kjambunathan.github.io/elpa/")
			 ))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(setq package-check-signature nil)

(package-initialize)

;; Refresh packages every timeout-seconds (probably 24h)
(let* ((ts-file (my/mkfpath package-user-dir ".last-refresh"))
       (last-ts (if (file-exists-p ts-file) (my/read-integer ts-file) 0))
       (timeout-seconds (* 24 60 60))
       (now (time-convert nil 'integer)))
  (when (> (- now last-ts) timeout-seconds)
    (package-refresh-contents) ;; update packages
    (with-temp-file ts-file ;; we did it, so update stored time stamp
      (insert (format "%d\n" now)))))

(my/ensure-package-installed 'quelpa)

(use-package quelpa
  :config (progn
	          (setq quelpa-upgrade-p t
		              quelpa-self-upgrade-p nil)))

(dolist (pkg (list 'use-package
                   'oauth2
                   'zoom-window
                   'darkroom
                   'queue
                   'clipetty
                   'graphviz-dot-mode
                   'csv-mode
                   'dedicated
                   'neotree
                   'gruber-darker-theme))
  
  (my/ensure-package-installed pkg))



(use-package oauth2)

(use-package zoom-window
  :config (progn (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
                 (custom-set-variables
                  '(zoom-window-mode-line-color "DarkGreen"))))

(defun toggle-darkroom-mode ()
  "Toggle mode to darkroom mode, to be invoked from some key combination."
  (interactive)
  (darkroom-tentative-mode nil))

(use-package darkroom
  :config (progn
            (setq darkroom-margins 0.15)
            (global-set-key (kbd "C-c d") 'toggle-darkroom-mode)))

;; Send emacs kill ring to remote clipboard
(use-package clipetty
  :config (global-clipetty-mode))


(add-to-list 'load-path (my/mkpath user-emacs-directory "org-mode" "lisp"))
(add-to-list 'load-path (my/mkpath user-emacs-directory "org-contrib" "lisp"))

(global-set-key (kbd "<C-f11>") 'toggle-frame-fullscreen)

;; general, shared settings divided into separate files
(let ((config-path (my/mkpath user-emacs-directory "config" "general")))
  (when (file-exists-p config-path)
    (add-to-list 'load-path config-path)
    (dolist (file-name (directory-files config-path))
	    (when (or (string-match-p "\.el$" file-name)
		            (string-match-p "\.el.gpg$" file-name))
        (let ((pkg (intern (file-name-base (file-name-base file-name)))))
          (require pkg))))))

;; User-specific settings (files containing secret things are pgp encrypted)
(let ((user-config-path (my/mkpath user-emacs-directory "config" my/user)))
  (when (file-exists-p user-config-path)
    (add-to-list 'load-path user-config-path)
    (dolist (file-name (directory-files user-config-path))
	    (if (or (string-match-p "\.el$" file-name)
		          (string-match-p "\.el.gpg$" file-name))
          (let ((pkg (intern (file-name-base (file-name-base file-name)))))
            (require pkg))))))


(use-package color)
(use-package graphviz-dot-mode :config (setq graphviz-dot-indent-width 2))
(use-package csv-mode)
(use-package dedicated)
(use-package neotree :config (setq neo-window-fixed-size nil))
(use-package gruber-darker-theme :config (global-clipetty-mode))


;; I like to copy stuff from web pages and make org mode documents out of them.
(defun html-to-org-region (&optional b e)
  "Convert HTML region to ORG format (standard input B and E)."
  (interactive "r")
  (shell-command-on-region b e "pandoc -f html -t markdown_github-raw_html | pandoc -f markdown -t org" (current-buffer) t))
;;  (comment-region (mark) (point)))
(global-set-key (kbd "C-c C-i C-o") 'html-to-org-region)

;; Navigation
;;(global-set-key (kbd "M-j") 'avy-goto-word-or-subword-1)
;;(global-set-key (kbd "C-v") 'yank) ; 【Ctrl+v - I compulsively hit this chord for "paste"】
;; Remap the window management keys to something more manageable
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Text scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'font-size-increase)
(global-set-key (kbd "C-_") 'font-size-decrease)
(global-set-key (kbd "C-)") 'font-size-default)

;; Enable reopening of recent files via C-x C-r
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/config/abbrev_defs")
(setq save-abbrevs t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; I like to see what time it is also when in full screen mode and OS menu bar is hidden
(display-time)

(winner-mode 1)


(provide 'general)
;;; general.el ends here
