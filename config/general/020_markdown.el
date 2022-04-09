;;; Package --- Summary:
;; Configuration for Markdown mode in Emacs.

;;; Commentary:

;;; Code:

(require 'defs)

;; Markdown mode
(progn
  (my/ensure-package-installed 'markdown-mode)
  (use-package markdown-mode
    :config (progn
	      (add-to-list 'auto-mode-alist '("\\.eml\\'" . markdown-mode)))))

(provide '020_markdown)
;;; 020_markdown.el ends here
