;;; Package --- Summary:

;;; Commentary:

;;; Code:

(require 'defs)

(dolist (pkg (list 'lsp-mode
                   'lsp-ui
                   'dap-mode
                   'which-key
                   'lsp-treemacs
                   'company
                   'magit
                   'focus
                   'flycheck
                   'flymake

                   ;; Modes that don't have their own config file:
                   'flycheck-grammarly
                   'flymake-grammarly
                   'flymake-eldev
                   'dockerfile-mode
                   'kubernetes
                   'yaml-mode
                   'json-mode
                   'csv-mode
                   ;;'yasnippet
		   ))
  
  (my/ensure-package-installed pkg))

(use-package magit
  :config
  (progn
    (setq magit-diff-use-overlays nil)
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x C-g") 'magit-status)))


(use-package flycheck
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :config (progn
            (global-company-mode)))
;;          (global-set-key (kbd "TAB") #'company-indent-or-complete-common)))

(use-package csv-mode
  :config (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package yaml-mode
  :config (progn
            (add-hook 'yaml-mode-hook
                      (lambda ()
                        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
            (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)))

(use-package dockerfile-mode)
(use-package kubernetes)

;;(use-package yasnippet)
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (dockerfile-mode . lsp)
         (yaml-mode . lsp)
         (json-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode)
         (lsp-mode . lsp-modeline-code-actions-mode)
         (lsp-managed-mode . lsp-modeline-diagnostics-mode)
         )
  :commands lsp
  :custom (;; set our custom config
           (lsp-headerline-breadcrumb-enable t)
           (lsp-ui-sideline-enable t)
           (lsp-modeline-code-actions-enable t)
           (lsp-ui-doc-enable t)
           (lsp-lens-enable t)
           ))


;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))


;; (add-hook 'clojure-mode-hook 'lsp)
;; (add-hook 'clojurescript-mode-hook 'lsp)
;; (add-hook 'clojurec-mode-hook 'lsp)

;; (add-hook 'yaml-mode-hook 'lsp)
;; (add-hook 'dockerfile-mode-hook 'lsp)
;; (add-hook 'html-mode-hook 'lsp)
;; (add-hook 'xml-mode-hook 'lsp)
;; (add-hook 'json-mode-hook 'lsp)

;; enable for all programming (not user friendly)
;;(add-hook 'prog-mode-hook 'lsp)
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

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       lsp-treemacs-sync-mode 1
;;       company-minimum-prefix-length 1
;;       lsp-lens-enable t
;;       lsp-signature-auto-activate nil

;;       lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
;;       lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp

;;       lsp-enable-symbol-highlighting 1
;;       lsp-ui-doc-enable 1
;;       lsp-eldoc-enable-hover 1
      
;;       )

(provide '010_ide)
;;; 010_ide.el ends here
