;;; Package --- Summary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; Commentary:

;;; Code:

(require 'mydefs)

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el
(progn (my/ensure-package-installed 'edit-server)
       (use-package edit-server)
       (require 'server)
       (unless (server-running-p) (server-start))
       (edit-server-start))

(provide '00_edit_server)
;;; 00_edit_server.el ends here
;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el
(progn
  (my/ensure-package-installed 'multi-term)
  (use-package multi-term
    :config (progn
	      (setq multi-term-program "zsh")
	      (add-hook 'term-mode-hook
			(lambda ()
			  (setq term-buffer-maximum-size 10000)))
	      (add-hook 'term-mode-hook
			(lambda ()
			  (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
			  (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))
	      (add-hook 'term-mode-hook
			(lambda ()
			  (define-key term-raw-map (kbd "C-y") 'term-paste))))))

(provide '012_multi_term)
;;; 012_multi_term.el ends here
;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el

;; expand-region
;; https://github.com/magnars/expand-region.el
(progn
  (my/ensure-package-installed 'expand-region)
  (use-package expand-region
    :config (progn
	      (global-set-key (kbd "C-=") 'er/expand-region))))

(provide '034_expand_region)
;;; 034_expand_region.el ends here
;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el

;; powerline
(progn
  (my/ensure-package-installed 'powerline)
  (use-package powerline
    :config (progn (powerline-default-theme))))

(provide '035_powerline)
;;; 035_powerline.el ends here

;;; Package --- Summary:

;;; Commentary:

;;; Code:

;; Alert when files have been externally modified
(defun mp/check-external-modifications ()
  (if (verify-visited-file-modtime (current-buffer))
      (setq header-line-format nil)
    ;;(if (buffer-modified-p (current-buffer))
	(setq header-line-format (format "*** WARNING [%s] WARNING ***"
					 (propertize "This file has been changed externally" 'face '(:foreground "#f92672")))))) ;;)
(run-with-timer 0 2 'mp/check-external-modifications)
;;
(provide '090_check_external_modifications)
;;; 090_check_external_modifications.el ends here
;;; Package --- Summary:

;;; Commentary:

;;; Code:

(defun mp/narrow-or-widen-dwim (p)
    "Works like distraction-free mode toggle. If the buffer is 
narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
	  ((region-active-p)
	   (narrow-to-region (region-beginning) (region-end)))
	  ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
	  (t (narrow-to-defun))))

(global-set-key (kbd "C-x =") 'mp/narrow-or-widen-dwim)
(provide '090_narrow_or_widen)
;;; 090_narrow_or_widen.el ends here
;;; Package --- Summary:

;;; Commentary:

;;; Code:

(defun my/delete-old-backup-files ()
  (interactive)
  (message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file)))))

(run-with-timer 0 86400 'my/delete-old-backup-files)

(provide '000_config)
;;; 000_config.el ends here
