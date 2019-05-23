
;; yaml-mode
(progn
  (ensure-package-installed 'yaml-mode)
  (use-package yaml-mode
    :config (progn
	      (add-hook 'yaml-mode-hook
			(lambda ()
			  (define-key yaml-mode-map "\C-m" 'newline-and-indent))))))

