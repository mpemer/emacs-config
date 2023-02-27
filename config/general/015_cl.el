;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; cl - common lisp development settings

(require 'defs)

(my/ensure-package-installed 'sly)
(use-package sly
  :config (setq inferior-lisp-program "ros -Q run"
                slime-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2000"))
                                             (roswell ("ros" "-Q" "run")))
                slime-default-lisp 'roswell))

;;  :config (setq inferior-lisp-program "sbcl"))

(my/ensure-package-installed 'sly-asdf)
(use-package sly-asdf)

;;(my/ensure-package-installed 'sly-contribs)
;;(use-package sly-asdf)

(my/ensure-package-installed 'sly-quicklisp)
(use-package sly-quicklisp)

(my/ensure-package-installed 'sly-repl-ansi-color)
(use-package sly-repl-ansi-color)


(provide '015_cl)
;; 015_cl.el ends here
