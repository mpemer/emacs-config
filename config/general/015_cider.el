;; cider
(progn
  (ensure-package-installed 'cider)
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
	      (add-hook 'cider-mode-hook #'eldoc-mode))))

