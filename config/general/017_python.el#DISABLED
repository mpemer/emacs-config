;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; python development settings

(require 'defs)

(dolist (pkg '(elpy
               flycheck-pyflakes
               flymake-python-pyflakes))
  
  (my/ensure-package-installed pkg))

(use-package elpy
  :init (advice-add 'python-mode :before 'elpy-enable)
  :hook (elpy-mode . (lambda () (add-hook 'before-save-hook 'elpy-format-code)))
  :config (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(setq python-indent-offset 4
      python-shell-interpreter "python"
      python-shell-interpreter-args "-i"
      ;;      python-shell-interpreter "jupyter"
      ;;      python-shell-interpreter-args "console --simple-prompt --no-confirm-exit --debug "
      python-shell-completion-native-enable nil
      python-shell-prompt-detect-failure-warning nil)

(elpy-enable)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(provide '017_python)
;;; 017_python.el ends here
