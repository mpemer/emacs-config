;;; Package --- Summary:

;;; Commentary:

;;; Code:

(require 'defs)

(dolist (pkg (list 'clojure-mode
                   'lsp-mode
                   'lsp-ui
                   'dap-mode
                   'which-key
                   'cider
                   'lsp-treemacs
                   'flycheck
                   'company
                   'flycheck-clj-kondo
                   'magit
                   'dockerfile-mode
                   'kubernetes
                   'yaml-mode
                   'csv-mode
                   'yasnippet))
  
  (my/ensure-package-installed pkg))

(use-package magit
  :config
  (progn
    (setq magit-diff-use-overlays nil)
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x C-g") 'magit-status)))

(use-package clojure-mode)

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
	          (add-hook 'cider-mode-hook #'turn-on-eldoc-mode)
	          (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
	          (add-hook 'cider-mode-hook #'eldoc-mode)
		  ;;(add-hook 'cider-mode-hook #'lsp)
		  ))

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

(use-package yasnippet)
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
