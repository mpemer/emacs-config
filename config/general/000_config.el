;;; Package --- Summary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; Commentary:

;;; Code:

(require 'defs)

;; Ensure exec-path-from-shell is installed and loaded
;; https://github.com/purcell/exec-path-from-shell
(progn
  (my/ensure-package-installed 'exec-path-from-shell)
  (use-package exec-path-from-shell
    :config
    (when (or (and (boundp 'daemonp) daemonp)
              (memq window-system '(mac ns x)))
      (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))
      (exec-path-from-shell-initialize))))


(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el
;; (progn (my/ensure-package-installed 'edit-server)
;;        (use-package edit-server)
;;        (require 'server)
;;        (unless (server-running-p) (server-start))
;;        (edit-server-start))

;; (progn
;;   (my/ensure-package-installed 'multi-term)
;;   (use-package multi-term
;;     :config (progn
;; 	      (setq multi-term-program "wsl")
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

;; expand-region
;; https://github.com/magnars/expand-region.el
;; (progn
;;   (my/ensure-package-installed 'expand-region)
;;   (use-package expand-region
;;     :config (progn
;; 	      (global-set-key (kbd "C-=") 'er/expand-region))))
;;(progn (my/ensure-package-installed 'edit-server)
;;       (use-package edit-server)
       (require 'server)
       (unless (server-running-p) (server-start))
;;       (edit-server-start))


;; powerline
;; https://github.com/milkypostman/powerline
(progn
  (my/ensure-package-installed 'powerline)
  (use-package powerline
    :config (powerline-default-theme)))


;; Warn if files have been externally modified.
;; Consider changing to this:
;; https://stackoverflow.com/questions/2284703/emacs-how-to-disable-file-changed-on-disk-checking/29556894#29556894
(progn
  (defun mp/check-external-modifications ()
    (if (verify-visited-file-modtime (current-buffer))
        (setq header-line-format nil)
	    (setq header-line-format
            (format "*** WARNING [%s] WARNING ***"
					          (propertize "This file has been changed externally"
                                'face '(:foreground "#f92672"))))))
  (run-with-timer 0 2 'mp/check-external-modifications))


;; Delete backup files, if they are older than one week
(progn
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
  (run-with-timer 0 86400 'my/delete-old-backup-files))


;; Set coding system
;; Set the default coding system to UTF-8
(let ((enc 'utf-8-unix))
  (prefer-coding-system enc)
  (set-default-coding-systems enc)
  (set-terminal-coding-system enc)
  (set-keyboard-coding-system enc)
  (setq default-buffer-file-coding-system enc

        ;; Suppress the coding system prompt
        coding-system-for-read enc
        coding-system-for-write enc

        ;; Ensure safe local variable values
        enable-local-variables :all

        ;; Suppress warning messages
        inhibit-eol-conversion t))

;; Function to remove ^M characters from the buffer
(defun remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR."
=======
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
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; Add the function to the find-file-hook
(add-hook 'find-file-hook 'remove-dos-eol)

(provide '000_config)
;;; 000_config.el ends here
