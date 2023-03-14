;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; cl - common lisp development settings

;;(require 'defs)

(defun roswell-configdir ()
  (substring (shell-command-to-string "ros roswell-internal-use version confdir") 0 -1))

(defun roswell-load (system)
  (let ((result (substring (shell-command-to-string
                            (concat "ros -L sbcl-bin -e \"(format t \\\"~A~%\\\" (uiop:native-namestring (ql:where-is-system \\\""
                                    system
                                    "\\\")))\"")) 0 -1)))
    (unless (equal "NIL" result)
      (load (concat result "roswell/elisp/init.el")))))

(defun roswell-opt (var)
  (with-temp-buffer
    (insert-file-contents (concat (roswell-configdir) "config"))
    (goto-char (point-min))
    (re-search-forward (concat "^" var "\t[^\t]+\t\\(.*\\)$"))
    (match-string 1)))

(defun roswell-directory (type)
  (concat
   (roswell-configdir)
   "lisp/"
   type
   "/"
   (roswell-opt (concat type ".version"))
   "/"))

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
      sly-default-lisp 'roswell
      inferior-lisp-program "ros dynamic-space-size=2048 -L sbcl -l ~/.sbclrc run")

(provide '015_cl)
;; 015_cl.el ends here
