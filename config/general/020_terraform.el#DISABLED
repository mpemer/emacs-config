;;; Package --- Summary:
;; Configuration for Terraform mode in Emacs.

;;; Commentary:

;;; Code:

(require 'defs)

;; Markdown mode
(progn
  (my/ensure-package-installed 'terraform-mode)
  (use-package terraform-mode
    :custom (terraform-indent-level 4)
    :config
    (defun my-terraform-mode-init ()
      ;; if you want to use outline-minor-mode
      (outline-minor-mode 1)
      )

    (add-hook 'terraform-mode-hook 'my-terraform-mode-init)))

(provide '020_terraform)
;;; 020_terraform.el ends here
