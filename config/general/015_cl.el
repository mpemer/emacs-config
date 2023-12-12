;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; cl - common lisp development settings

;;(require 'defs)

;; Define the path to the Roswell helper file
(defvar roswell-helper-file "~/.roswell/helper.el")

;; Check if the Roswell helper file exists and load it
(if (file-exists-p roswell-helper-file)
    (load roswell-helper-file))

(my/ensure-package-installed 'sly)
(use-package sly)

(my/ensure-package-installed 'sly-named-readtables)
(use-package sly-named-readtables)


(add-to-list 'load-path (roswell-directory "sly"))
(require 'sly-autoloads)

(my/ensure-package-installed 'sly-asdf)
(use-package sly-asdf)

;;(my/ensure-package-installed 'sly-contribs)
;;(use-package sly-asdf)

(my/ensure-package-installed 'sly-quicklisp)
(use-package sly-quicklisp)

(my/ensure-package-installed 'sly-repl-ansi-color)
(use-package sly-repl-ansi-color)

(setq inferior-lisp-program "ros dynamic-space-size=2048 -L sbcl -l ~/.sbclrc run"
      sly-lisp-implementations '((sbcl ("sbcl" "dynamic-space-size=2048" "-l" "~/.sbclrc"))
                                 (roswell ("ros" "dynamic-space-size=2048" "-L" "sbcl" "-l" "~/.sbclrc" "run")))
      sly-default-lisp 'roswell)

(provide '015_cl)
;; 015_cl.el ends here
