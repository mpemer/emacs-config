;; expand-region
;; https://github.com/magnars/expand-region.el
(progn
  (ensure-package-installed 'expand-region)
  (use-package expand-region
    :config (progn
	      (global-set-key (kbd "C-=") 'er/expand-region))))

