(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;;; Packages that must be loaded by straight-use-package
(dolist (pkg '(use-package ;; Integrate straight with use-package
		el-patch ;; Enable custom git repository references in use-package
		org ;; Ensure latest version of org
		))  
  (straight-use-package pkg))


;;;; Pre-configurations, needed by packages before loading

;; Make it so we don't have to use `:straight t` in use-package below:
(setq straight-use-package-by-default t)

;; Define the path to the Roswell helper file
(defvar roswell-helper-file "~/.roswell/helper.el"
  "Path to Roswell helper.el file.")

(defun my/load-roswell-helper (&rest _args) ;; _args ignored; required to be used in advice below
  "Load the Roswell helper file and update the load-path for SLY if not already loaded."
  (when (and (file-exists-p roswell-helper-file)
             (not (featurep 'roswell-helper)))
    (load roswell-helper-file)
    (add-to-list 'load-path (roswell-directory "sly"))))

(advice-add 'sly :before #'my/load-roswell-helper)


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

(defvar my/user (user-login-name))
(defvar my/home (getenv "HOME"))

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





;;;; Load packages, with configs as needed

(dolist (pkg '(all-the-icons
	       gruber-darker-theme
	       oauth2
	       queue
	       dedicated
	       dap-mode
               focus
               flycheck
               flymake
	       flylisp
               flycheck-grammarly
               flymake-grammarly
               flymake-eldev
               dockerfile-mode
               kubernetes
               json-mode
	       pandoc
	       sly
	       sly-named-readtables
	       sly-asdf
	       sly-quicklisp
	       sly-repl-ansi-color
               org-caldav  ;; https://github.com/dengste/org-caldav
	       org-contrib
	       ox-pandoc
	       ox-epub
	       ox-hugo
	       edit-server
	       ;;ob-emacs-lisp
	       ;;ob-lisp
	       ;;ob-org
	       ;;ob-shell
	       ;;ob-sql
	       ))
  (eval `(use-package ,pkg)))

(use-package powerline
  :config (powerline-default-theme))

(use-package darkroom
  :config (progn
            (setq darkroom-margins 0.1)
            (global-set-key (kbd "C-c d") 'darkroom-mode)))

(use-package zoom-window
  :config (progn (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
                 (custom-set-variables
                  '(zoom-window-mode-line-color "DarkGreen"))))

(use-package eterm-256color
  :config (add-hook 'term-mode-hook #'eterm-256color-mode))

(use-package aweshell
  :straight (aweshell :type git :host github :repo "manateelazycat/aweshell"))

(use-package magit
  :config   (progn
	      (setq magit-diff-use-overlays nil)
	      (global-set-key (kbd "C-x g") 'magit-status)
	      (global-set-key (kbd "C-x C-g") 'magit-status)))

(use-package company
    :defer 0.1
    :config
    (global-company-mode t)
    (setq-default
        company-idle-delay 0.05
        company-require-match nil
        company-minimum-prefix-length 0

        ;; get only preview
        company-frontends '(company-preview-frontend)
        ;; also get a drop down
        company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
        ))

(use-package markdown-mode
  :config (add-to-list 'auto-mode-alist '("\\.eml\\'" . markdown-mode)))

(use-package csv-mode
  :config (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package yaml-mode
  :config (progn
            (add-hook 'yaml-mode-hook
                      (lambda ()
                        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
            (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)))

;; EPUB Reader
(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (dockerfile-mode . lsp)
         (yaml-mode . lsp)
         (json-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode)
         (lsp-mode . lsp-modeline-code-actions-mode)
         (lsp-managed-mode . lsp-modeline-diagnostics-mode)
         )
  :commands lsp
  :custom (;; set our custom config
           (lsp-headerline-breadcrumb-enable t)
           (lsp-ui-sideline-enable t)
           (lsp-modeline-code-actions-enable t)
           (lsp-ui-doc-enable t)
           (lsp-lens-enable t)
           ))


;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))


;; https://github.com/sabof/org-bullets
(use-package org-bullets
    :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))



;;;; Specific Requirements

(dolist (it '(sly-autoloads org-agenda
			    ob-emacs-lisp ob-lisp
			    ob-org ob-shell ob-sql
			    calendar
			    holidays
			    server
			    ))
  (require it))


;;;; Configurations

(unless (display-graphic-p)
  (xterm-mouse-mode 1))



(setq inferior-lisp-program "ros dynamic-space-size=2048 -L sbcl -l ~/.sbclrc run"
      sly-lisp-implementations '((sbcl ("sbcl" "dynamic-space-size=2048" "-l" "~/.sbclrc"))
                                 (roswell ("ros" "dynamic-space-size=2048" "-L" "sbcl" "-l" "~/.sbclrc" "run")))
      sly-default-lisp (if (file-exists-p roswell-helper-file)
			   'roswell
			 'sbcl))

(global-set-key (kbd "<C-f11>") 'toggle-frame-fullscreen)
;; Navigation
(windmove-default-keybindings) ;; Move between windows with shift-arrows

;; ctrl-shift-arrows to resize windows, but also add c-x chords to support remove phone sessions
;; Remap the window management keys to something more manageable
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x C-j") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "C-x C-m") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x C-i") 'enlarge-window)

;; Text scaling
(global-set-key (kbd "C-}") 'text-scale-increase)
(global-set-key (kbd "C-{") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'font-size-increase)
(global-set-key (kbd "C-_") 'font-size-decrease)
(global-set-key (kbd "C-)") 'font-size-default)

;; Marking text with blutooth keyboard
(global-set-key (kbd "C-x x") 'set-mark-command)


(setq display-time-mode t
      tool-bar-mode nil
      menu-bar-mode nil)

;; Enable reopening of recent files via C-x C-r
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(setq-default abbrev-mode t)
(let ((abbrev-file "~/.emacs.d/abbrev_defs"))
  (when (file-exists-p abbrev-file)
    (read-abbrev-file abbrev-file)))
(setq save-abbrevs t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; I like to see what time it is also when in full screen mode and OS menu bar is hidden
(display-time)



(setq org-export-with-toc nil)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c C-b") 'org-switchb)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "C-c c") 'org-capture)


;; Shortcut to inserting structured templates, like code blocks (list below)
(global-set-key (kbd "C-c C-.") 'org-insert-structure-template)
(setq org-structure-template-alist
  '(("a" . "export ascii\n")
    ("c" . "center\n")
    ("C" . "comment\n")
    ("e" . "example\n")
    ("E" . "export")
    ("h" . "export html\n")
    ("l" . "export latex\n")
    ("q" . "quote\n")
    ("s" . "src")
    ("v" . "verse\n")))


;; Keep deep levels of priorities
(setq org-lowest-priority ?F
      org-default-priority ?B)


;;;; REFILE

;; Targets include this file and any file contributing to the agenda - up to 3 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))


;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)


(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)
;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Adds a CLOSED property time stamp to the TODO entry if marked as done
(setq org-log-done 'time
      org-log-repeat 'time
      org-log-into-drawer t)

;;;; END REFILE

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "FILE(f)" "IP(i)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
              (sequence "MEETING(m)" "EVENT(e)"))))

(setq org-caldav-todo-percent-states
      (quote ((0 "TODO") (0 "FILE") (50 "IP") (100 "DONE")
                           (:calendar-id "marcus@pemer.com"
                            :files ("~/org/tasks.org")
                            :inbox "~/org/tasks.org"
                 )

              (0 "WAITING") (0 "HOLD") (0 "CANCELLED")
              (0 "MEETING") (0 "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light gray" :weight bold)
              ("FILE" :foreground "light blue" :weight bold)
              ("IP" :foreground "light blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
	            ("EVENT" :foreground "light blue" :weight bold))))

;; Change item state with C-c C-t
(setq org-use-fast-todo-selection 'expert)

;; Change item state with Shift-[left|right]
(setq org-treat-S-cursor-todo-selection-as-state-change t)

;; Place tags close to the right-hand side of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

;;;; Do not dim blocked tasks
;;(setq org-agenda-dim-blocked-tasks nil)

;;;; Compact the block agenda view
;;(setq org-agenda-compact-blocks t)

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with TAG."
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;;; END AGENDA


(setq org-agenda-start-day "-7d"
      org-agenda-span 35
      org-agenda-start-on-weekday 1
      org-agenda-log-mode 'closed)

(setq org-id-search-archives nil)

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(with-eval-after-load 'org
  ;;(setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (setq org-src-tab-acts-natively t)
  (add-hook 'org-mode-hook (lambda ()
			     (visual-line-mode)
			     (flyspell-mode))))

;; By default, save openoffice exports as ms word documents
(setq org-export-odt-preferred-output-format "docx")
(setq org-odt-preferred-output-format "docx")


;;(setq org-enable-hugo-support

;; Make it so that Agenda view stops warning about deadlines once item has been scheduled
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (org-save-all-org-buffers)))

;; I like to copy stuff from web pages and make org mode documents out of them.
(defun html-to-org-region (&optional b e)
  "Convert HTML region to ORG format (standard input B and E)."
  (interactive "r")
  (shell-command-on-region b e "pandoc -f html -t markdown_github-raw_html | pandoc -f markdown -t org" (current-buffer) t))
;;  (comment-region (mark) (point)))
(global-set-key (kbd "C-c C-i C-o") 'html-to-org-region)



;;
;; Inject logic that adds .org extenions to path of org file links that have no suffix,
;; but where the target file exists
;;
(defun org-link-open-as-file (path arg)
  "Pretend PATH is a file name and open it.

According to \"file\"-link syntax, PATH may include additional
search options, separated from the file name with \"::\".

This function is meant to be used as a possible tool for
`:follow' property in `org-link-parameters'."
  (let* ((option (and (string-match "::\\(.*\\)\\'" path)
		                  (match-string 1 path)))
	       (file-name (if (not option) path
		                  (substring path 0 (match-beginning 0))))
         (org-file-name (concat (string-trim-right file-name "\\.") ".org"))
         (alt-file-name (if (and (= 0 (length (file-name-extension file-name)))
                                 (not (file-exists-p file-name)))
;;                                 (file-exists-p org-file-name))
                            org-file-name
                          file-name)))

    (if (string-match "[*?{]" (file-name-nondirectory file-name))
	      (dired file-name)
      (apply #'org-open-file
	     alt-file-name
	     arg
	     (cond ((not option) nil)
		         ((string-match-p "\\`[0-9]+\\'" option)
		          (list (string-to-number option)))
		         (t (list nil option)))))))


(defun my/org-next-business-day (date)
  "Calculate the next business day after DATE, excluding weekends and holidays."
  (let ((next-date (calendar-gregorian-from-absolute
                    (+ 1 (calendar-absolute-from-gregorian date)))))
    (message "Calculating next business day from %s" date)
    (while (or (member (calendar-day-of-week next-date) '(0 6)) ; 0 = Sunday, 6 = Saturday
               (holiday-p next-date))
      (setq next-date (calendar-gregorian-from-absolute
                       (+ 1 (calendar-absolute-from-gregorian next-date)))))
    (message "Next business day is %s" next-date)
    next-date))

(defun my/org-format-date (date)
  "Format DATE as an org-mode date string."
  (format "[%04d-%02d-%02d %s]"
          (nth 2 date) (nth 0 date) (nth 1 date)
          (calendar-day-name date)))

(defun my/org-get-next-scrum-date ()
  "Calculate the next business day for the next scrum date."
  (let ((today (calendar-current-date)))
    (message "Today's date is %s" today)
    (my/org-format-date (my/org-next-business-day today))))

(defun my/org-get-journal-file-path ()
  "Get the path to the journal.org file in a platform-agnostic way."
  (expand-file-name "~/org/journal.org"))

(defun my/org-get-last-scrum-date-from-journal ()
  "Retrieve the most recent scrum date from journal.org."
  (message "Retrieving last scrum date from journal.org")
  (with-current-buffer (find-file-noselect (my/org-get-journal-file-path))
    (goto-char (point-max))
    (if (re-search-backward "^\\*\\{3\\} \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) " nil t)
        (let ((date (org-read-date nil t (match-string-no-properties 1))))
          (message "Last scrum date is %s" date)
          date)
      (error "No valid scrum date found in journal.org"))))

(defun my/org-get-done-and-ip-tasks ()
  "Get tasks completed or in progress since the last scrum across all agenda files."
  (let ((last-scrum-date (my/org-get-last-scrum-date-from-journal))
        (agenda-files (org-agenda-files)))
    (message "Getting done and IP tasks since %s" (format-time-string "%Y-%m-%d" last-scrum-date))
    (dolist (file agenda-files)
      (message "Processing file: %s" file)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((heading (nth 4 (org-heading-components))))
             (message "Task done or in progress: %s" heading)
             (insert (format "- [X] %s\n" heading))))
         (format "CLOSED>=\"%s\"|+IP" (format-time-string "%Y-%m-%d" last-scrum-date)))))))

(defun my/org-get-next-tasks ()
  "Get tasks scheduled until the next scrum across all agenda files."
  (let ((next-scrum-date (org-read-date nil nil (my/org-get-next-scrum-date)))
        (agenda-files (org-agenda-files)))
    (message "Getting next tasks until %s" (format-time-string "%Y-%m-%d" next-scrum-date))
    (dolist (file agenda-files)
      (message "Processing file: %s" file)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((heading (nth 4 (org-heading-components))))
             (message "Next task: %s" heading)
             (insert (format "- %s\n" heading))))
         (format "+SCHEDULED<=\"%s\"" (format-time-string "%Y-%m-%d" next-scrum-date)))))))

(defun my/org-get-current-goals-from-journal ()
  "Retrieve the Current Goals from the last entry in journal.org."
  (message "Retrieving Current Goals from journal.org")
  (with-current-buffer (find-file-noselect (my/org-get-journal-file-path))
    (goto-char (point-max))
    (when (re-search-backward "^\\*\\* Current Goals" nil t)
      (let ((start (point)))
        (forward-paragraph)
        (let ((goals (buffer-substring-no-properties start (point))))
          (message "Current Goals found: %s" goals)
          goals)))))

(defun my/org-update-scrum-section ()
  "Update the Scrum section with done and next tasks."
  (interactive)
  (message "Updating Scrum section")
  (save-excursion
    (org-find-exact-headline-in-buffer "Scrum")
    (outline-next-visible-heading 1)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (outline-next-visible-heading 1)
    (delete-region (point) (progn (outline-next-visible-heading 1) (point)))
    (insert "** Current Goals\n")
    (let ((current-goals (my/org-get-current-goals-from-journal)))
      (when current-goals
        (insert current-goals)))
    (insert "** Since Last\n")
    (message "Starting to get done and IP tasks")
    (my/org-get-done-and-ip-tasks)
    (message "Finished getting done and IP tasks")
    (insert "** Until Next\n")
    (message "Starting to get next tasks")
    (my/org-get-next-tasks)
    (message "Finished getting next tasks")
    (insert "** Impediments\n")
    (insert "** Notes\n")
    (widen))
  (message "Scrum section updated"))

(defun my/org-copy-to-journal ()
  "Copy the Scrum section to journal.org under today's date."
  (interactive)
  (message "Copying Scrum section to journal.org")
  (let ((scrum-section (save-excursion
                         (org-find-exact-headline-in-buffer "Scrum")
                         (org-copy-subtree))))
    (with-current-buffer (find-file-noselect (my/org-get-journal-file-path))
      (goto-char (point-max))
      (unless (org-find-entry-with-id (format-time-string "%Y-%m-%d")))
        (org-insert-heading)
        (insert (format-time-string "%Y-%m-%d")))
      (org-narrow-to-subtree)
      (goto-char (point-max))
      (insert scrum-section)
      (widen)
      (save-buffer))
  (message "Scrum section copied to journal.org"))


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



(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

;; Ensure emacs can handle pgp key passowrds using loopback
(setq epa-pinentry-mode 'loopback)


;; User-specific settings (files containing secret things are pgp encrypted)
(let ((user-config-path (my/mkpath user-emacs-directory "profiles" (user-login-name))))
  (when (file-exists-p user-config-path)
    (add-to-list 'load-path user-config-path)
    (dolist (file-name (directory-files user-config-path))
      (if (string-match-p "\.el$" file-name)
          (require (intern (file-name-base (file-name-base file-name))))
	(when (string-match-p "\.el.gpg$" file-name)
	  (load file-name))))))


;;;; chatgpt

(use-package chatgpt-shell
  :config
  (progn
    (setq chatgpt-shell-api-url "https://api.openai.com/v1/chat/completions")
    (setq chatgpt-shell-model "o3-mini")))
;;    (setq chatgpt-shell-model "gpt-o3-mini-high")))

(global-set-key (kbd "C-c r") 'chatgpt-shell-send-region)
(global-set-key (kbd "C-c e") 'chatgpt-shell-quick-insert)


;;;; chatgpt_end












;; Keep custom config under config directory (a git repo).
;; Specify the file to save custom settings
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
;; Load the custom settings from the file, if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;;(require 'edit-server)
(unless (and (fboundp 'edit-server-start) (process-status "edit-server"))
  (edit-server-start))

(unless (server-running-p) (server-start))

