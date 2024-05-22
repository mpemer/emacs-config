;;; Package --- Summary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; Commentary:

;;; Code:

;; Keep custom config under config directory (a git repo).
;; Entry point to config repo is general.el
(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'defs)

(dolist (pkg '(all-the-icons
               gruber-darker-theme))
  (my/ensure-package-installed pkg))


(use-package all-the-icons)
(use-package gruber-darker-theme)

(setq backup-directory-alist (list (cons "." (concat user-emacs-directory "tmp"))))

; ;; For Windows, set exec-path from PATH environment variable
; (when (eq system-type 'windows-nt)
;   (let ((path (split-string (getenv "PATH") path-separator)))
;     (setq exec-path path)
;     (setenv "PATH" (mapconcat 'identity path path-separator))))
; (if (daemonp)
;    (add-hook 'after-make-frame-functions
;              (lambda (frame)
;                (with-selected-frame frame
;                  (set-cursor-color "#eebbaa"))))
;  (set-cursor-color "#eebbaa"))

;; Set the path to your custom file
(setq custom-file (expand-file-name "local.el" user-emacs-directory))

;; Load the custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

(require 'general)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide '.init)
;;; init.el ends here
