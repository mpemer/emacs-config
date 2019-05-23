;; magit
(progn
  (ensure-package-installed 'magit)
  (use-package magit
    :config
    (progn
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x C-g") 'magit-status))))
