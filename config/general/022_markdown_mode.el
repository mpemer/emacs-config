;; Markdown mode
(progn
  (ensure-package-installed 'markdown-mode)
  (use-package markdown-mode
    :config (progn
	      (add-to-list 'auto-mode-alist '("\\.eml\\'" . markdown-mode)))))

