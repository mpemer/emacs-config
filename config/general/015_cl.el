;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; cl - common lisp development settings

;;(require 'defs)

(dolist (pkg '(flylisp))  
  (my/ensure-package-installed pkg))

;; N.B. You need to have roswell installed
;; ideally with a source-compiled sbcl
;; and "ros install sly"
;; after this, you should have helper.el available

;; Define the path to the Roswell helper file
;;(defvar roswell-helper-file "~/.roswell/helper.el")

;; Check if the Roswell helper file exists and load it
;;(when (file-exists-p roswell-helper-file)
;;    (load roswell-helper-file)
;;    (add-to-list 'load-path (roswell-directory "sly"))
;;    )

;; Define the path to the Roswell helper file in a platform-independent way
(defvar roswell-helper-file
  (expand-file-name (concat (getenv "HOME") "/.roswell/helper.el")))

;; Function to determine the Roswell installation directory
(defun ros-dir ()
  (if-let ((roswell-bin (executable-find "ros")))
      (file-name-directory roswell-bin)))

;;(expand-file-name "lisp/sly" (ros-dir))

;; Check if the Roswell helper file exists and load it
(when (file-exists-p roswell-helper-file)
  (load roswell-helper-file)
  (if-let ((rd (ros-dir)))
      (add-to-list 'load-path (expand-file-name "lisp/sly" rd))))

(my/ensure-package-installed 'sly)
(use-package sly)

(my/ensure-package-installed 'sly-named-readtables)
(use-package sly-named-readtables)

;;(my/ensure-package-installed 'sly-autoloads)
(require 'sly-autoloads)

(my/ensure-package-installed 'sly-asdf)
(use-package sly-asdf)

;;(my/ensure-package-installed 'sly-contribs)
;;(use-package sly-contribs)

(my/ensure-package-installed 'sly-quicklisp)
(use-package sly-quicklisp)

(my/ensure-package-installed 'sly-repl-ansi-color)
(use-package sly-repl-ansi-color)

(setq inferior-lisp-program "ros dynamic-space-size=2048 -L sbcl -l ~/.sbclrc run"
      sly-lisp-implementations '((sbcl ("sbcl" "dynamic-space-size=2048" "-l" "~/.sbclrc"))
                                 (roswell ("ros" "dynamic-space-size=2048" "-L" "sbcl" "-l" "~/.sbclrc" "run")))
      sly-default-lisp (if (file-exists-p roswell-helper-file)
			   'roswell
			 'sbcl))

(provide '015_cl)
;; 015_cl.el ends here
