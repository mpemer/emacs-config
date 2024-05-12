;;; Package --- Summary:

;;; Commentary:

;;; Code:

(require 'defs)

(dolist (pkg (list 'clojure-mode
                   'cider
                   'flycheck-clojure
                   'flycheck-clj-kondo))
  
  (my/ensure-package-installed pkg))

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

(use-package flycheck-clj-kondo :config (require 'flycheck-clj-kondo))

(require 'lsp-mode)
;; enable for all programming (not user friendly)
;;(add-hook 'prog-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(provide '016_clojure)
;;; 016_clojure.el ends here
