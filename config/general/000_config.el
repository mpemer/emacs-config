;;; Package --- Summary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; Commentary:

;;; Code:

(require 'defs)

(progn
  (my/ensure-package-installed 'exec-path-from-shell)
  (use-package exec-path-from-shell
    :config
    (when (or (daemonp)
              (memq window-system '(mac ns x)))
      (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))
      (exec-path-from-shell-initialize))))


(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el
;;(progn (my/ensure-package-installed 'edit-server)
;;       (use-package edit-server)
       (require 'server)
       (unless (server-running-p) (server-start))
;;       (edit-server-start))


;; powerline
(progn
  (my/ensure-package-installed 'powerline)
  (use-package powerline
    :config (progn (powerline-default-theme))))

(defun mp/check-external-modifications ()
  "Check if any buffers have been modified externally, and if so, prompt an action from user."
  (if (verify-visited-file-modtime (current-buffer))
      (setq header-line-format nil)
    ;;(if (buffer-modified-p (current-buffer))
	(setq header-line-format (format "*** WARNING [%s] WARNING ***"
					 (propertize "This file has been changed externally" 'face '(:foreground "#f92672")))))) ;;)
(run-with-timer 0 2 'mp/check-external-modifications)

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


(defun mp/toggle-frame-undecorated (p)
  "Toggle frame title bar on or off.  The parameter P is not used."
    (interactive "P")
    (declare (interactive-only))
    (set-frame-parameter nil 'undecorated
			 (not (frame-parameter nil 'undecorated))))
  
(global-set-key (kbd "C-x W") 'mp/toggle-frame-undecorated)

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
