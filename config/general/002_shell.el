;;; Package --- Summary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; Commentary:

;;; Code:

(require 'defs)

;; (progn
;;   (my/ensure-package-installed 'multi-term)
;;   (use-package multi-term
;;     :config (progn
;; 	      (setq multi-term-program "fish")
;; 	      (add-hook 'term-mode-hook
;; 			(lambda ()
;; 			  (setq term-buffer-maximum-size 10000)))
;; 	      (add-hook 'term-mode-hook
;; 			(lambda ()
;; 			  (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
;; 			  (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))
;; 	      (add-hook 'term-mode-hook
;; 			(lambda ()
;; 			  (define-key term-raw-map (kbd "C-y") 'term-paste))))))

(use-package eterm-256color
  :ensure t)

(add-hook 'term-mode-hook #'eterm-256color-mode)

(use-package aweshell
  :quelpa (abc-mode :fetcher github :repo "manateelazycat/aweshell"))


(provide '002_shell)
;;; 002_shell.el ends here
