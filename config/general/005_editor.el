;;; Package --- Summary:

;;; Commentary:

;;; Code:

(require 'defs)

(dolist (pkg (list 'oauth2
                   'queue
                   'zoom-window
                   'darkroom
                   'clipetty
                   'graphviz-dot-mode
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


(global-set-key (kbd "<C-f11>") 'toggle-frame-fullscreen)

(use-package color)
(use-package graphviz-dot-mode :config (setq graphviz-dot-indent-width 2))
(use-package dedicated)
(use-package neotree :config (setq neo-window-fixed-size nil))
(use-package gruber-darker-theme :config (global-clipetty-mode))


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

;;;;;;;;;;;;;;;xxx
(use-package magit
  :config
  (progn
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x C-g") 'magit-status)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package cider
  :config (progn
	          (global-set-key (kbd "<insert>") 'cider-pprint-eval-defun-at-point)
	          (setq cider-show-error-buffer 'only-in-repl)
	          (setq cider-lein-parameters "repl :headless :host localhost")
	          ;;(setq cider-jdk-src-paths '("~/src/clojure"
		        ;;			  "~/src/openjdk-8"))
	          (setq cider-font-lock-dynamically '(macro core function var))
	          (setq cider-overlays-use-font-lock t)
	          (setq cider-clojure-cli-global-options "-A:dev")
	          (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
	          (add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
	          (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
	          (add-hook 'cider-mode-hook #'eldoc-mode)))

(use-package flycheck
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clj-kondo
  :config (require 'flycheck-clj-kondo))

(use-package company
  :config (progn
            (global-company-mode)))
;;          (global-set-key (kbd "TAB") #'company-indent-or-complete-common)))

(use-package csv-mode)
(use-package yaml-mode
  :config (progn
            (add-hook 'yaml-mode-hook
                      (lambda ()
                        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
            (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)))

(use-package dockerfile-mode)
(use-package kubernetes)



(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

(provide '005_editor)
;;; 005_editor.el ends here
