;;; Package --- Summary:
;; Org mode configuration for Emacs

;;; Commentary:

;;; Code:

(require 'defs)

;;(add-to-list 'load-path (my/mkpath user-emacs-directory "org-mode" "lisp"))
;;(add-to-list 'load-path (my/mkpath user-emacs-directory "org-contrib" "lisp"))

(require 'org-agenda)


(let ((package-list '(
                      ;;org-alert   ;; https://github.com/spegoraro/org-alert
		                  org-bullets ;; https://github.com/sabof/org-bullets
                      org-caldav  ;; https://github.com/dengste/org-caldav
                      ;;async       ;; https://github.com/jwiegley/emacs-async
                      ;;org-contrib
                      ;;org-beautify-theme
		                  )))
  (dolist (package package-list)
    (progn
      (my/ensure-package-installed package)
      (use-package package))))

;;(my/ensure-package-installed package)
;;(require 'org-contrib)
(let ((package-list '(ox-pandoc   ;; https://github.com/kawabata/ox-pandoc
		                  ox-epub     ;; https://github.com/ofosos/ox-epub
		                  ;;ox-odt      ;; https://github.com/kjambunathan/org-mode-ox-odt/blob/master/README.md
		                  ox-hugo     ;; https://ox-hugo.scripter.co/
                      )))
  (dolist (package package-list)
    (progn
			(my/ensure-package-installed package)
			(use-package package
				:ensure t
				:after ox))))

(with-eval-after-load 'ox
  (require 'ox-hugo))

;;(use-package ox-extra
;;    :config
;;    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; (use-package ox-latex
;;     :config
;;     ;; code here will run after the package is loaded
;;     (setq org-latex-pdf-process
;;           '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;             "bibtex %b"
;;             "pdflatex -interaction nonstopmode -output-directory %o %f"
;;             "pdflatex -interaction nonstopmode -output-directory %o %f"))
;;     (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
;;     ;; (setq org-latex-prefer-user-labels t)

;;     ;; deleted unwanted file extensions after latexMK
;;     (setq org-latex-logfiles-extensions
;;           (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

;;     (unless (boundp 'org-latex-classes)
;;       (setq org-latex-classes nil)))

;;(use-package org-beautify-theme)
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ob-clojure
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
;;(progn
;;  (use-package ob-clojure
;;    :config (progn
;;	      (setq org-babel-clojure-backend 'cider))))

(progn
  ;;(use-package ob-clojure)
  (use-package ob-emacs-lisp)
  ;;(use-package ob-java)
  (use-package ob-lisp)
  (use-package ob-org)
  (use-package ob-shell)
  (use-package ob-sql)
  ;;(use-package ob-sh)
  ;;(use-package ob-css)
  ;;(use-package ob-js)
  ;;(use-package ob-python)
  ;;(use-package ob-ruby)
  ;;(use-package ob-sass)
  ;;(use-package ob-sed)
  ;;(use-package ob-)
)

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

;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING") ("HOLD" . t))
;;               (done ("WAITING") ("HOLD"))
;;               ("TODO" ("WAITING") ("IP") ("CANCELLED") ("HOLD"))
;;               ("PLAN" ("WAITING") ("IP")("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("IP")("CANCELLED") ("HOLD")))))


;; (defun elisp-showdoc (f)
;;   "Show docstring for F in notification area."
;;   (interactive (list (thing-at-point 'symbol t)))
;;   (message
;;    "%s"
;;    (let* ((doc-list      (split-string (documentation (intern f)) "\n"))
;;           (number-lines  (min (- (floor (* max-mini-window-height (frame-height))) 2)
;;                               (- (length doc-list) 2)))
;;           (subset        (concatenate 'list
;;                                       (last doc-list)
;;                                       '("")
;;                                       (subseq doc-list 0 number-lines)))
;;           (pruned-subset (if (string-equal (car (last subset)) "")
;;                              (butlast subset)
;;                              subset)))
;;      (mapconcat #'identity pruned-subset "\n"))))
;;;; AGENDA

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


(require 'calendar)
(require 'holidays)

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

;;(global-set-key (kbd "C-c s u") 'my/org-update-scrum-section)
;;(global-set-key (kbd "C-c s j") 'my/org-copy-to-journal)


(provide '060_org)
;;; 060_org.el ends here
