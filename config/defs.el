;;; Package --- Summary:

;; Referenced by .emacs config files.

;;; Commentary:

;; Declare some helper variables, functions, suppress warnings etc

;;; Code:

;; There are some precompilation warnings that are suppressed
;; only by vacuously declaring some variables.
(eval-when-compile
  (defvar url-http-extra-headers ())
  (defvar url-http-method ())
  (defvar url-http-data ())
  (defvar oauth--token-data ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ()))

(setq
 ;; Suppress warnings
 ;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
 ad-redefinition-action 'accept

 ;; Prefer TLS1.3
 gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defvar my/user (getenv "USER"))

(defun my/mkfpath (&rest segs)
  "Combine string segments SEGS into file path."
  (expand-file-name (car (last segs)) ;; last element is a file name
                    (apply 'concat ;; all elements up to last are directories
                           (mapcar 'file-name-as-directory
                                   (butlast segs)))))

(defun my/mkpath (&rest segs)
  "Combine string segments SEGS into path."
  (apply 'concat (mapcar 'file-name-as-directory segs)))

(defun my/ensure-package-installed (package)
  "Ensure that PACKAGE is installed."
  (unless (package-installed-p package) (package-install package)))

(defun my/read-integer (file)
  "Read integer from FILE."
  (string-to-number
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))

(defun my/pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(provide 'defs)
;;; defs.el ends here
