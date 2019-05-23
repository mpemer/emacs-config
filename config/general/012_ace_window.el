;; ace-window
(progn
  (ensure-package-installed 'ace-window)
  (use-package ace-window
    :config (progn
	      (global-set-key (kbd "M-z") 'ace-window))))
