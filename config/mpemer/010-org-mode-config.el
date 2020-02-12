;; ORG-MODE CONFIG CHANGES

;; First make sure the packages are installed
(let ((package-list '(
		      org-alert
		      org-beautify-theme		      
		      org-ehtml
		      org-jira
		      org-pdfview
		      ox-pandoc ;; https://github.com/kawabata/ox-pandoc
		      ox-asciidoc
		      ox-clip
		      ox-epub
		      ox-jira
		      ox-minutes
		      ox-slack
		      ox-twbs
		      ox-odt ;; https://github.com/kjambunathan/org-mode-ox-odt/blob/master/README.md
		      )))
  (dolist (package package-list) (progn
				   (ensure-package-installed package)
				   (use-package package))))

(progn
  (ensure-package-installed 'org-gcal)
  (use-package org-gcal
    :config (progn
	     (setq org-gcal-client-id "441016108337-1hupr92oqr0kbk71uiuhe377ji6n6pqm.apps.googleusercontent.com"
		   org-gcal-client-secret "GdpE7SUxUWXbMiOG9USuIpsA"
		   org-gcal-file-alist '(("pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" .  "~/org/plan.org"))
		   org-gcal-header-alist '(("441016108337-1hupr92oqr0kbk71uiuhe377ji6n6pqm.apps.googleusercontent.com" . "#+PROPERTY: TIMELINE_FACE \"pink\"\n"))
		   org-gcal-auto-archive nil
		   org-gcal-notify-p nil)
	     (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
	     (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch))))

;;					  ("another-mail@gmail.com" .  "~/task.org")))))

;;(progn
;;  (ensure-package-installed 'org-ac)
;;  (use-package org-ac
;;    :config (progn
;;	      (org-ac/config-default))))

;; org-bullets
;; https://github.com/sabof/org-bullets
;;(progn
;;  (ensure-package-installed 'org-bullets)
;;  (use-package org-bullets
;;    :config (progn
;;	      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))))

;; org-mime
;; https://orgmode.org/worg/org-contrib/org-mime.html
;;(progn
;;  (ensure-package-installed 'org-mime)
;;  (use-package org-mime
;;    :config (progn
;;	      (setq org-mime-library 'mml))))
;;
;;(require 'ox-confluence)

;; ob-clojure
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
(progn
;;  (ensure-package-installed 'ob-clojure)
  (use-package ob-clojure
    :config (progn
	      (setq org-babel-clojure-backend 'cider))))

;; ob-clojure
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
(progn
;;  (ensure-package-installed 'ob-clojure)
;;  (use-package ob-sh)
  (use-package ob-clojure)
  (use-package ob-css)
  (use-package ob-emacs-lisp)
  (use-package ob-java)
  (use-package ob-js)
  (use-package ob-lisp)
  (use-package ob-org)
  (use-package ob-python)
  (use-package ob-ruby)
  (use-package ob-sass)
  (use-package ob-sed)
  (use-package ob-shell)
  (use-package ob-sql)  
;;  (use-package ob-)
)

(org-babel-do-load-languages
 'org-babel-load-languages '(
			     (clojure . t)
			     (css . t)
			     (emacs-lisp . t)
			     (java . t)
			     (js . t)
			     (lisp . t)
			     (org . t)
			     (python . t)
			     (ruby . t)
			     (sass . t)
			     (sed . t)
			     (shell . t)
			     (sql . t)
			     ))


(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)
(setq org-latex-listings-options '(("breaklines" "true")))

(setf org-enable-reveal-js-support nil)

;; org-crypt settings
;; I don't use this much anymore, but keeping it
;; for backwards compatibility, until I have
;; removed all usage instances of it
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

;;(comment
(setq org-export-with-toc nil)
(add-hook 'org-mode-hook #'visual-line-mode)

;; (defun my/org-inline-css-hook (exporter)
;;   "Insert custom inline css to automatically set the
;; background of code to whatever theme I'm using's background"
;;   (when (eq exporter 'html)
;;     (let* ((my-pre-bg (face-background 'default))
;;            (my-pre-fg (face-foreground 'default)))
;;       (setq
;;        org-html-head-extra
;;        (concat
;;         org-html-head-extra
;;         (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
;;                 my-pre-bg my-pre-fg))))))

;; (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(defun my/org-print-wrap-hook (exporter)
  (when (eq exporter 'html)
    "@media print {
       pre {
         white-space: pre-wrap;
       }
      }"))

(add-hook 'org-export-before-processing-hook 'my/org-print-wrap-hook)

;; If I wanted to remove the above and instead use css styling for exported code blocks
(setq org-html-htmlize-output-type 'css)
;;(setq org-html-htmlize-output-type 'inline-css)


(defun my/reformat-for-scrum-notes ()
  "Reformat org to confluence scrum notes"
  (interactive)
  (goto-char 0)
  (replace-regexp "^- " "*# ")
  (goto-char 0)
  (replace-regexp "^h1. " "* ")
  (goto-char 0)
  (replace-regexp "^h2. " "** ")
  (goto-char 0)
  (replace-regexp "^h3. " "*** ")
  (goto-char 0)
  (replace-regexp "^h4. " "**** "))

(defun my/to-scrum-notes ()
  "Convert region to scrum notes format"
  (interactive)
  (org-confluence-export-as-confluence)
  (switch-to-buffer "*org CONFLUENCE Export*")
  (my/reformat-for-scrum-notes)
  (goto-char 0)
  (flush-lines  "\<20..-..-..")
  (goto-char 0)
  (flush-lines  "^$")
  (goto-char 0)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; right-alt+w
(global-set-key (kbd "∑") 'my/to-scrum-notes)



(defun todo-to-int (todo)
  "Convert todo item to int value, for sorting"
    (first (-non-nil
            (mapcar (lambda (keywords)
                      (let ((todo-seq
                             (-map (lambda (x) (first (split-string  x "(")))
                                   (rest keywords)))) 
                        (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                    org-todo-keywords))))

(defun my/org-sort-key ()
  "Assign an integer sort value to org entries based on type, priority and date"
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority))
         (deadline (org-entry-get (point) "DEADLINE"))
         (scheduled (org-entry-get (point) "SCHEDULED"))
         (timestamp (org-entry-get (point) "TIMESTAMP"))
         (closed (org-entry-get (point) "CLOSED"))
         (dstr (or closed deadline scheduled timestamp "<3000-00-00>"))
	 (keystr (format "%03d %03d %s%s%s"
			 todo-int
			 priority-int
			 (substring dstr 1 5)
			 (substring dstr 6 8)
			 (substring dstr 9 11)
			 )))
    (message keystr)
    keystr))


(defun my/org-sort-entries ()
  "Sort ORG entries according to my rules"
  (interactive)
  (org-sort-entries nil ?f #'my/org-sort-key))

(global-set-key (kbd "C-c os") 'my/org-sort-entries)

(setq org-directory "~/org")

(defun mp-org-notes ()
  (interactive)
  (find-file (concat org-directory "/notes.org")))
(defun mp-org-plan ()
  (interactive)
  (find-file (concat org-directory "/plan.org")))
(defun mp-org-bookmarks ()
  (interactive)
  (find-file (concat org-directory "/bookmarks.org")))
(defun mp-emacs ()
  (interactive)
  (find-file "~/.emacs.d/config/.emacs"))

;;(current-time)
     
(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)

(defun my/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'my/org-sort-key))

;; By default, archive into the same file name under archive subfolder, but fold items into datetree
(setq org-archive-location (concat "archive/%s::datetree/"))
;;(setq org-archive-location (concat org-directory "/journal/archive.org::datetree/*"))
;;#+ARCHIVE: %s_archive.org::datetree/*

;;(setq package-check-signature nil)
;;(require 'org-gcal)
(defun my/org-home (filename)
  (concat "~/org/" filename))

;; oauth2

;;(progn
;;  (ensure-package-installed 'oauth2)
;;  (use-package oauth2
;;    :config (progn
;;	      (setq plstore-cache-passphrase-for-symmetric-encryption t))))

;; org-caldav-oauth2-providers '((google
		    ;; 				   "https://accounts.google.com/o/oauth2/v2/auth"
		    ;; 				   "https://www.googleapis.com/oauth2/v4/token"
		    ;; 				   "https://www.googleapis.com/auth/calendar"
		    ;; 				   "https://apidata.googleusercontent.com/caldav/v2/%s/events"))))))


;; (setq org-caldav-calendars
;;   '((:calendar-id "pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" :files ("~/org/plan.org")
;;      :inbox "~/org/refile.org")))

;; (let* ((path "~/src/org-caldav")
;;        (filename (concat path "/org-caldav.el")))
;;   (if (file-exists-p filename)
;;       (progn
;; 	(add-to-list 'load-path path)
;; 	(require 'org-caldav)
;; 	(setq org-caldav-url 'google
;; 	      org-icalendar-timezone "UTC"
;; 	      org-caldav-oauth2-client-id "931083343613-1u4fv2nb4tf1ktgq2678it8o3jtj3qlq.apps.googleusercontent.com"
;; 	      org-caldav-oauth2-client-secret "4TDz6ByKh341Ik3mzHU7wAlB"
;; 	      org-caldav-inbox "~/org/plan.org"
;; 	      org-caldav-files '("~/org/tasks.org" "~/org/plan.org"))
;; 	      ;;org-caldav-calendars '((:calendar-id "my-calendar-id-from-google" :files ("~/org/plan.org"))))

;; 	(defun my/org-caldav-sync ()
;; 	  (interactive)
;; 	  (let ((message-log-max))
;; 	    (message "Synchronizing calendars..."))
;; 	  (let ((remaining-retries 3))
;; 	    (while (> remaining-retries 0)
;; 	      (condition-case ex                  ;
;; 		  (progn
;; 		    (org-caldav-sync)
;; 		    (setq remaining-retries 0)) ;; all done
;; 		('error
;; 		 (progn 
;; 		   (if (string-match-p "https://apidata.googleusercontent.com/caldav/v2.*401 Unauthorized"
;; 				       (error-message-string ex))
;; 		       (progn
;; 			 (kill-matching-buffers-no-ask
;; 			  "^ \\*http apidata\\.googleusercontent\\.com:443\\*.*" t)
;; 			 (let ((message-log-max))
;; 			   (message "Retrying to synchronize calenders..."))
;; 			 ;; There was a synchronization error, most likely due to an
;; 			 ;; expired oauth2 access token. Trying again should work fine.
;; 			 (sleep-for 1)
;; 			 (setq remaining-retries (- remaining-retries 1)))
;; 		     (error "%s" (error-message-string ex)))))))))

;; 	(global-set-key (kbd "C-c oS") 'my/org-caldav-sync))))


(setq prj-folders '("pemer" "mercury" "iteego" "kohler" "mrmaster" "personal"))

(setq org-directory "~/org"
      org-agenda-files (cons "~/org" (mapcar (lambda (folder) (concat "~/" folder "/org")) prj-folders))
      org-default-notes-file "~/org/notes.org"
      org-icalendad-timezone "Europe/Wien")

(setq org-capture-templates
      (quote (("t" "TODO" entry (file "~/org/notes.org")
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("r" "Respond" entry (file "~/org/notes.org")
	       "* NEXT Respond to %:from on %:subject\n:PROPERTIES:\n:SCHEDULED: %t\n%U\n:END:\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	      ("n" "Note" entry (file "~/org/notes.org")
	       "* %? :NOTE:\n:PROPERTIES:\n:CREATED: %t\n:END:\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("m" "Meeting" entry (file "~/org/notes.org")
	       "* MEETING with %? :MEETING:\n:PROPERTIES:\n:SCHEDULED: %t\n:END:\n%U" :clock-in t :clock-resume t)
	      ("p" "Phone call" entry (file "~/org/notes.org")
	       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c C-b") 'org-switchb)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "C-c c") 'org-capture)

;;(global-set-key (kbd "<f12>") 'org-agenda)
;;(global-set-key (kbd "<f5>") 'bh/org-todo)
;;(global-set-key (kbd "<S-f5>") 'bh/widen)
;;(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
;;(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;;(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
;;(global-set-key (kbd "<f9> b") 'bbdb)
;;(global-set-key (kbd "<f9> c") 'calendar)
;;(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;;(global-set-key (kbd "<f9> g") 'gnus)
;;(global-set-key (kbd "<f9> h") 'bh/hide-other)
;;(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

;;(global-set-key (kbd "<f9> I") 'bh/punch-in)
;;(global-set-key (kbd "<f9> O") 'bh/punch-out)

;;(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

;;(global-set-key (kbd "<f9> r") 'boxquote-region)
;;(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

;;(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
;;(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

;;(global-set-key (kbd "<f9> v") 'visible-mode)
;;(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
;;(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
;;(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
;;(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
;;(global-set-key (kbd "<f11>") 'org-clock-goto)
;;(global-set-key (kbd "C-<f11>") 'org-clock-in)
;;(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish))
;;(global-set-key (kbd "C-c c") 'org-capture)

;; (defun bh/hide-other ()
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading 'invisible-ok)
;;     (hide-other)
;;     (org-cycle)
;;     (org-cycle)
;;     (org-cycle)))

;; (defun bh/set-truncate-lines ()
;;   "Toggle value of truncate-lines and refresh window display."
;;   (interactive)
;;   (setq truncate-lines (not truncate-lines))
;;   ;; now refresh window display (an idiom from simple.el):
;;   (save-excursion
;;     (set-window-start (selected-window)
;;                       (window-start (selected-window)))))

;; (defun bh/make-org-scratch ()
;;   (interactive)
;;   (find-file "/tmp/publish/scratch.org")
;;   (gnus-make-directory "/tmp/publish"))

;; (defun bh/switch-to-scratch ()
;;   (interactive)
;;   (switch-to-buffer "*scratch*"))

;; Remove empty LOGBOOK drawers on clock out
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;;;; REFILE

; Targets include this file and any file contributing to the agenda - up to 3 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))



; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


;;;; Refile settings
;;;; Exclude DONE state tasks from refile targets
;;(defun bh/verify-refile-target ()
;;  "Exclude todo keywords with a done state from refile targets"
;;  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
;;
;;(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;;; END REFILE

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "IP(p)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "darkred" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("IP" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)
	      ("EVENT" :foreground "blue" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("IP") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("IP")("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("IP")("CANCELLED") ("HOLD")))))


;;;; AGENDA

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
;; (setq org-agenda-custom-commands
;;       (quote (("N" "Notes" tags "NOTE"
;;                ((org-agenda-overriding-header "Notes")
;;                 (org-tags-match-list-sublevels t)))
;;               (" " "Agenda"
;;                ((agenda "" nil)
;;                 (tags "REFILE"
;;                       ((org-agenda-overriding-header "Tasks to Refile")
;;                        (org-tags-match-list-sublevels nil)))
;;                 (tags-todo "-CANCELLED/!"
;;                            ((org-agenda-overriding-header "Stuck Projects")
;;                             (org-agenda-skip-function 'bh/skip-non-stuck-projects)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-HOLD-CANCELLED/!"
;;                            ((org-agenda-overriding-header "Projects")
;;                             (org-agenda-skip-function 'bh/skip-non-projects)
;;                             (org-tags-match-list-sublevels 'indented)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-CANCELLED/!NEXT"
;;                            ((org-agenda-overriding-header (concat "Project Next Tasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
;;                             (org-tags-match-list-sublevels t)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-sorting-strategy
;;                              '(todo-state-down effort-up category-keep))))
;;                 (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
;;                            ((org-agenda-overriding-header (concat "Project Subtasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-non-project-tasks)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
;;                            ((org-agenda-overriding-header (concat "Standalone Tasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-project-tasks)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-CANCELLED+WAITING|HOLD/!"
;;                            ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-non-tasks)
;;                             (org-tags-match-list-sublevels nil)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
;;                 (tags "-REFILE/"
;;                       ((org-agenda-overriding-header "Tasks to Archive")
;;                        (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
;;                        (org-tags-match-list-sublevels nil))))
;;                nil))))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;;; END AGENDA


; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("IP" . ?i)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("ORG" . ?O)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)



;; ;;;; PHONE

;; (require 'bbdb)
;; (require 'bbdb-com)

;; (global-set-key (kbd "<f9> p") 'bh/phone-call)

;; ;;
;; ;; Phone capture template handling with BBDB lookup
;; ;; Adapted from code by Gregory J. Grubbs
;; (defun bh/phone-call ()
;;   "Return name and company info for caller from bbdb lookup"
;;   (interactive)
;;   (let* (name rec caller)
;;     (setq name (completing-read "Who is calling? "
;;                                 (bbdb-hashtable)
;;                                 'bbdb-completion-predicate
;;                                 'confirm))
;;     (when (> (length name) 0)
;;       ; Something was supplied - look it up in bbdb
;;       (setq rec
;;             (or (first
;;                  (or (bbdb-search (bbdb-records) name nil nil)
;;                      (bbdb-search (bbdb-records) nil name nil)))
;;                 name)))

;;     ; Build the bbdb link if we have a bbdb record, otherwise just return the name
;;     (setq caller (cond ((and rec (vectorp rec))
;;                         (let ((name (bbdb-record-name rec))
;;                               (company (bbdb-record-company rec)))
;;                           (concat "[[bbdb:"
;;                                   name "]["
;;                                   name "]]"
;;                                   (when company
;;                                     (concat " - " company)))))
;;                        (rec)
;;                        (t "NameOfCaller")))
;;     (insert caller)))


;; ;;;; END PHONE



(with-eval-after-load 'org       
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'flyspell-mode))

;; By default, save openoffice exports as ms word documents
(setq org-export-odt-preferred-output-format "docx")
(setq org-odt-preferred-output-format "docx")
