;;; Package --- Summary:
;; Configuration for reading of EPUB file format in Emacs.

;;; Commentary:

;;; Code:
(require 'defs)

;; EPUB Reader
(my/ensure-package-installed 'nov)
(use-package nov
  :config (progn
	          (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))))


(provide '030_epub)
;;; 030_epub.el ends here
