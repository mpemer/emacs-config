;; Packages that don't have individual configs go here
(let ((package-list
       '(
	 dockerfile-mode
	 k8s-mode
	 kubernetes
	 )))
  (dolist (package package-list)
    (progn (ensure-package-installed package)
	   (use-package package))))
