;; EPUB Reader
(progn
  (ensure-package-installed 'nov)
  (use-package nov
    :config (progn
	      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))))


