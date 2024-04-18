;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; python development settings

;;(require 'defs)

(my/ensure-package-installed 'elpy)
(use-package elpy)
(elpy-enable)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(provide '017_python)
;; 017_python.el ends here
