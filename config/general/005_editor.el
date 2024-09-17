;;; Package --- Summary:

;;; Commentary:

;;; Code:

(require 'defs)

(dolist (pkg (list 'oauth2
                   'queue
                   'zoom-window
                   'darkroom
                   ;;'clipetty
                   ;;'graphviz-dot-mode
                   ;;'dedicated
                   ))
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
            (setq darkroom-margins 0.1)
            (global-set-key (kbd "C-c d") 'toggle-darkroom-mode)))

;; Send emacs kill ring to remote clipboard
;;(use-package clipetty
;;  :config (global-clipetty-mode))


(global-set-key (kbd "<C-f11>") 'toggle-frame-fullscreen)

;;(use-package color)
;;(use-package graphviz-dot-mode :config (setq graphviz-dot-indent-width 2))
;;(use-package dedicated)

;; Navigation
;;(global-set-key (kbd "M-j") 'avy-goto-word-or-subword-1)
;;(global-set-key (kbd "C-v") 'yank) ; 【Ctrl+v - I compulsively hit this chord for "paste"】
;; Remap the window management keys to something more manageable
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Text scaling
(global-set-key (kbd "C-}") 'text-scale-increase)
(global-set-key (kbd "C-{") 'text-scale-decrease)
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

(provide '005_editor)
;;; 005_editor.el ends here
