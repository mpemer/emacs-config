;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; cl - common lisp development settings

(require 'defs)

(my/ensure-package-installed 'slime)
(use-package slime
  :config (progn
            (setq inferior-lisp-program "sbcl")
            (add-hook 'slime-connected-hook
		                  (lambda ()
		                    (when (slime-eval `(cl:if (cl:find-package :cl21-user) t))
		                      (slime-repl-set-package :cl21-user)
		                      (slime-repl-eval-string "(cl21:enable-cl21-syntax)"))) t)))


(provide '015_cl)
;;; 015_cl.el ends here
