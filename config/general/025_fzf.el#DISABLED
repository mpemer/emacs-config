;;; Package --- Summary:
;; Configuration for FZF mode in Emacs.

;;; Commentary:

;;; Code:

(require 'defs)

;; FZF mode
(when (executable-find "fzf")
  (my/ensure-package-installed 'fzf)
  (use-package fzf
    :config
    (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
          fzf/executable "fzf"
          fzf/git-grep-args "-i --line-number %s"
          ;; command used for `fzf-grep-*` functions
          ;; example usage for ripgrep:
          ;; fzf/grep-command "rg --no-heading -nH"
          fzf/grep-command "grep -nrH"
          ;; If nil, the fzf buffer will appear at the top of the window
          fzf/position-bottom t
          fzf/window-height 15)))

(provide '025_fzf)
;;; 025_fzf.el ends here
