;; ORG-MODE CONFIG CHANGES

(require 'org-depend)
(require 'org-mime)
(require 'org-secretary)
(require 'ox-confluence)
(require 'ox-deck)
(require 'ox-taskjuggler)
(require 'ox-s5)

;; org-crypt settings
;; I don't use this much anymore, but keeping it
;; for backwards compatibility, until I have
;; removed all usage instances of it
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))


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


(define-key global-map "\C-cc" 'org-capture)

(defun mp-org-notes ()
  (interactive)
  (find-file (concat org-directory "/notes.org")))
(defun mp-org-plan ()
  (interactive)
  (find-file (concat org-directory "/plan.org")))
(defun mp-org-tasks ()
  (interactive)
  (find-file (concat org-directory "/tasks.org")))
(defun mp-org-goals ()
  (interactive)
  (find-file (concat org-directory "/goals.org")))
(defun mp-org-archive ()
  (interactive)
  (find-file (concat org-directory "/archive.org)")))
(defun mp-emacs ()
  (interactive)
  (find-file "~/.emacs.d/config/.emacs"))

(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c oo") 'mp-org-plan)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c oa") 'mp-org-archive)
(global-set-key (kbd "C-c OO") 'mp-org-archive)
(global-set-key (kbd "C-c oe") 'mp-emacs)

(defun my/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'my/org-sort-key))

;;(setq package-check-signature nil)
;;(require 'org-gcal)
(defun my/org-home (filename)
  (concat "~/org/" filename))

(let* ((path "~/src/org-caldav")
       (filename (concat path "/org-caldav.el")))
  (if (file-exists-p filename)
      (progn
	(add-to-list 'load-path path)
	(require 'org-caldav)

	(setq plstore-cache-passphrase-for-symmetric-encryption t

	      org-directory "~/org"
	      org-caldav-save-directory "~/org/.org-caldav-state"
	      ;;"~/org/tasks.org"
	      org-agenda-files '("~/org/notes.org"
				 "~/org/plan.org"
				 "~/org/journal.org")
	      
	      org-default-notes-file "~/org/notes.org"
	      
	      org-icalendad-timezone "Europe/Wien"
	      
	      org-capture-templates
	      '(( "t" "Task" entry (file+headline "~/org/plan.org" "Tasks") "** TODO [#A] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
		("a" "Appointment" entry (file+headline "~/org/plan.org" "Plan") "** %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
		("g" "Goal" entry (file+headline "~/org/plan.org" "Goals") "** %?\n%u" :prepend t)
		("n" "Note" entry (file+headline "~/org/notes.org" "Notes") "** %?\n%u" :prepend t)
		("i" "Idea" entry (file+headline "~/org/notes.org" "Ideas") "** %?\n%T" :prepend t)
		("l" "Link" entry (file+headline "~/org/notes.org" "Links") "** %? %^L %^g \n%T" :prepend t)
		("j" "Journal" entry (file+datetree "~/org/journal.org") "* %?\nEntered on %U\n  %i\n  %a")))
	
	;; org-caldav configuration - we use either this or org-gcal
	(setq org-caldav-url 'google
	      org-caldav-inbox "~/org/plan.org"
	      org-caldav-files '("~/org/plan.org"))
	      ;;org-caldav-calendars '((:calendar-id "my-calendar-id-from-google" :files ("~/org/plan.org"))))

	(defun my/org-caldav-sync ()
	  (interactive)
	  (let ((message-log-max))
	    (message "Synchronizing calendars..."))
	  (let ((remaining-retries 3))
	    (while (> remaining-retries 0)
	      (condition-case ex                  ;
		  (progn
		    (org-caldav-sync)
		    (setq remaining-retries 0)) ;; all done
		('error
		 (progn 
		   (if (string-match-p "https://apidata.googleusercontent.com/caldav/v2.*401 Unauthorized"
				       (error-message-string ex))
		       (progn
			 (kill-matching-buffers-no-ask
			  "^ \\*http apidata\\.googleusercontent\\.com:443\\*.*" t)
			 (let ((message-log-max))
			   (message "Retrying to synchronize calenders..."))
			 ;; There was a synchronization error, most likely due to an
			 ;; expired oauth2 access token. Trying again should work fine.
			 (sleep-for 1)
			 (setq remaining-retries (- remaining-retries 1)))
		     (error "%s" (error-message-string ex)))))))))

	(global-set-key (kbd "C-c oS") 'my/org-caldav-sync))))
