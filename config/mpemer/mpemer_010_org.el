;;; -*- mode: emacs-lisp  -*-
;; Package --- Summary:

;;; Commentary:

;;; Code:


;; ORG-MODE CONFIG CHANGES

;;; Code:
(require 'defs)

(setq org-directory (my/mkpath my/home "org"))

(defun mp-org-tasks ()
  "Open tasks.org."
  (interactive)
  (find-file (expand-file-name "tasks.org" org-directory)))

(defun mp-org-notes ()
  "Open notes.org."
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory)))

(defun mp-org-meetings ()
  "Open meetings.org."
  (interactive)
  (find-file (expand-file-name "meetings.org" org-directory)))

(defun mp-org-plan ()
  "Open plan.org."
  (interactive)
  (find-file (expand-file-name "plan.org" org-directory)))

(defun mp-org-goals ()
  "Open goals.org."
  (interactive)
  (find-file (expand-file-name "goals.org" org-directory)))

(defun mp-org-ideas ()
  "Open ideas.org."
  (interactive)
  (find-file (expand-file-name "ideas.org" org-directory)))

(defun mp-org-family ()
  "Open family.org."
  (interactive)
  (find-file (expand-file-name "calendars/family.org" org-directory)))

(defun mp-org-journal ()
  "Open journal.org."
  (interactive)
  (find-file (expand-file-name "journal.org" org-directory)))

(defun mp-org-system ()
  "Open system.org."
  (interactive)
  (find-file (expand-file-name "system.org" org-directory)))

(defun mp-org-bookmarks ()
  "Open bookmarks.org."
  (interactive)
  (find-file (expand-file-name "bookmarks.org" org-directory)))

(global-set-key (kbd "C-c oo") 'mp-org-tasks)
(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c om") 'mp-org-meetings)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c og") 'mp-org-goals)
(global-set-key (kbd "C-c oi") 'mp-org-ideas)
(global-set-key (kbd "C-c oj") 'mp-org-journal)
(global-set-key (kbd "C-c of") 'mp-org-family)
(global-set-key (kbd "C-c os") 'mp-org-system)

(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)


;; By default, archive into the same file name under archive subfolder, but fold items into datetree
;; (defun my/org-archive-path (dir filename)
;;   "Create a full path for DIR and FILENAME, appending '_archive' before the file extension."
;;   (let* ((file (file-name-nondirectory filename))
;;          (name (file-name-sans-extension file))
;;          (ext (file-name-extension file))
;;          (archive-file (concat name "_archive." ext)))
;;     (concat (file-name-as-directory dir) archive-file)))

;; (setq org-archive-location (my/org-archive-path "archive" "%s::datetree"))
(setq org-archive-location "archive/%s_archive::datetree")


(setq org-directory (expand-file-name "~/org")
      org-agenda-files (append (list (expand-file-name "tasks.org" org-directory)
                                     (expand-file-name "plan.org" org-directory)
                                     (expand-file-name "goals.org" org-directory))
                               (directory-files-recursively
                                (expand-file-name "calendars" org-directory) "\\.org$"))
      org-caldav-save-directory (expand-file-name ".caldav" org-directory)
      org-caldav-exclude-tags '("no_sync")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-icalendar-timezone "CET"
      plstore-cache-passphrase-for-symmetric-encryption t

      org-caldav-sync-todo nil
      org-icalendar-include-todo nil
      org-icalendar-use-deadline '(event-if-not-todo event-if-todo)
      org-icalendar-use-scheduled '(event-if-not-todo event-if-todo)

      org-caldav-debug-level 0

      org-agenda-custom-commands
      '(("o" "Hide meds"
         ((agenda "")
          (alltodo ""))
         ((org-agenda-files (remove (expand-file-name "calendars/meds.org" org-directory) (org-agenda-files)))))))


(let ((org-dir (file-name-as-directory org-directory)))
  (setq org-capture-templates
        `(("t" "Task" entry (file ,(concat org-dir "tasks.org"))
 	         "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
DEADLINE: %t
SCHEDULED: 
")
 	        ("n" "Note" entry (file ,(concat org-dir "notes.org"))
 	         "* %?
:PROPERTIES:
:CREATED: %U
:END:
")
 	        ("i" "Idea" entry (file+headline ,(concat org-dir "ideas.org") "Ideas")
 	         "* %?
:PROPERTIES:
:CREATED: %U
:END:
")
 	        ("g" "Goal" entry (file ,(concat org-dir "goals.org"))
 	         "* %?
:PROPERTIES:
:CREATED: %U
:END:
")
 	        ("m" "Meeting" entry (file ,(concat org-dir "meetings.org"))
 	         "* MEETING with %?
:PROPERTIES:
:CREATED: %U
:SCHEDULED: %t
:END:
")
 	        ("j" "Journal" entry (file+datetree ,(concat org-dir "journal.org"))
 	         "* Scrum
:PROPERTIES:
:CREATED: %U
:END:
** Current Goals
    1. 

** Since Last [/]
    - [ ]

** Until Next
    - [ ]

** Impediments
    - 

** Notes

")
          )))



;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ;;("@errand" . ?e)
                            ;;("@office" . ?o)
                            ;;("@home" . ?H)
                            ("business" . ?b)
                            ("personal" . ?p)
                            ("flag"     . ??)
                            (:endgroup))))


(defun my/org-caldav-sync ()
  (interactive)
  (let ((message-log-max))
    (message "Synchronizing calendars..."))
  (let ((remaining-retries 10))
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
                 (kill-matching-buffers
                  "^ \\*http apidata\\.googleusercontent\\.com:443\\*.*" :no-ask t)
                 (let ((message-log-max))
                   (message "Retrying to synchronize calendars..."))
                 ;; There was a synchronization error, most likely due to an
                 ;; expired oauth2 access token. Trying again should work fine.
                 (sleep-for 1)
                 (setq remaining-retries (- remaining-retries 1)))
             (error "%s" (error-message-string ex))))))))
  (cl-letf (((symbol-function 'kill-buffer--possibly-save) (lambda (buffer) t)))
    (kill-matching-buffers "^org-caldav-*" :no-ask t))
  (message "Calendar sync complete."))


(global-set-key "\C-cs" 'my/org-caldav-sync)

(defun my/remove-id-properties-from-archive ()
  "Remove all :ID: properties from org entries in files under /org/archive."
  (interactive)
  (message "Starting to remove :ID: properties from archive...")
  (let ((files (directory-files-recursively (my/mkpath org-directory "archive") "\\.org$"))
        (files-processed 0)
        (total-ids-removed 0))
    (dolist (file files)
      (let ((ids-removed 0))
        (message "Processing file: %s" file)
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (goto-char (point-min))
          (while (re-search-forward "^:ID:" nil t)
            (setq ids-removed (1+ ids-removed))
            (org-delete-property "ID"))
          (when (> ids-removed 0)
            (save-buffer))
          (kill-buffer))
        (setq files-processed (1+ files-processed))
        (setq total-ids-removed (+ total-ids-removed ids-removed))
        (message "Removed %d :ID: properties from %s" ids-removed file)))
    (message "Finished processing %d files. Total :ID: properties removed: %d"
             files-processed total-ids-removed)))


(add-hook 'org-load-hook 'my/remove-id-properties-from-archive)
(advice-add 'org-archive-subtree :after #'my/remove-id-properties-from-archive)

(defun diary-second-thursday-of-month (date)
  "Return `t` if DATE is the second Thursday of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (day-of-week (calendar-day-of-week date)))
    (and (= day-of-week 4)  ; Thursday in Emacs Lisp starts from 0 (Sunday) so Thursday is 4
         (<= 8 day)         ; Second Thursday can't be earlier than the 8th
         (<= day 14))))     ; and not later than the 14th


(setq org-feed-alist
      '(("Slashdot"
         "http://rss.slashdot.org/Slashdot/slashdot"
         "~/org/feeds.org" "Slashdot")))
;;        ("NYT"
;;         "http://rss.art19.com/the-daily"
;;         "~/org/feeds.org" "NYT")))


;;
;;
;; SCRUM LOGIC
;;
(require 'calendar)
(require 'holidays)
(require 'org)

(setq org-id-link-to-org-use-id 'use-existing)

(defun my/org-next-business-day (date)
  "Calculate the next business day after DATE, excluding weekends and holidays."
  (let ((next-date (calendar-gregorian-from-absolute
                    (+ 1 (calendar-absolute-from-gregorian date)))))
    (message "Calculating next business day from %s" date)
    (while (or (member (calendar-day-of-week next-date) '(0 6)) ; 0 = Sunday, 6 = Saturday
               (calendar-check-holidays next-date))
      (setq next-date (calendar-gregorian-from-absolute
                       (+ 1 (calendar-absolute-from-gregorian next-date)))))
    (message "Next business day is %s" next-date)
    next-date))

(defun my/org-format-date (date)
  "Format DATE as an 'org-mode' date string."
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
  (let ((journal-file (my/org-get-journal-file-path)))
    (message "Journal file path: %s" journal-file)
    (with-current-buffer (find-file-noselect journal-file)
      (goto-char (point-max))
      (if (re-search-backward "^ *:CREATED: *\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]]+\\)\\]" nil t)
          (let ((date (match-string-no-properties 1)))
            (message "Last scrum date is %s" date)
            date)
        (error "No valid scrum date found in journal.org")))))

;;(my/org-get-last-scrum-date-from-journal)

(defun my/org-timestamp-to-string (timestamp)
  "Convert TIMESTAMP element to a string."
  (cond
   ((stringp timestamp) timestamp)
   ((and (listp timestamp) (eq (car timestamp) 'timestamp))
    (org-element-interpret-data timestamp))
   (t (error "Invalid timestamp: %s" timestamp))))

(defun my/org-make-element-link (element buffer-file-name)
  "Make a link to ELEMENT task in BUFFER-FILE-NAME."
  (let* ((title (org-element-property :raw-value element))
         (id (org-element-property :ID element))
         (file-name (file-relative-name buffer-file-name org-directory)))
    (message "Making link for: %s" title)
    (if id ;; if we already have an ID defined, then use it
        (org-link-make-string (concat "id:" id) title)
      (org-link-make-string (concat "file:" file-name "::" title) title))))

(defun my/status-equals (element status)
  "Check if the status of the Org task ELEMENT equals STATUS."
  (string= (org-element-property :todo-keyword element) status))

(defun my/org-process-file-for-done-and-ip (file last-scrum-date)
  "Process a single file FILE for done and IP tasks since LAST-SCRUM-DATE.
Returns a list of strings of tasks with links."
  (with-current-buffer (find-file-noselect file)
    (let ((parsed-buffer (org-element-parse-buffer)))
      (org-element-map parsed-buffer '(headline inlinetask)
        (lambda (element)
          (let* ((closed (org-element-property :closed element))
                 (closed-date (when closed
                                (org-time-string-to-absolute
                                 (my/org-timestamp-to-string closed))))
                 (link (my/org-make-element-link element (buffer-file-name)))
                 (closed-since-last (and closed-date
                                         (>= closed-date (org-time-string-to-absolute last-scrum-date))))
                 (in-progress (my/status-equals element "IP")))
            (when (or closed-since-last in-progress)
              (let ((formatted-string (format "- [%s] %s" (if closed "X" "-") link)))
                (set-text-properties 0 (length formatted-string) nil formatted-string)
                formatted-string))))
        nil nil nil t))))


(defun my/org-get-done-and-ip-tasks ()
  "Get tasks completed or in progress since the last scrum across all agenda files.
Returns a string of tasks with links."
  (let ((last-scrum-date (my/org-get-last-scrum-date-from-journal))
        (agenda-files (seq-filter
                       (lambda (file) (not (string-match-p "meds.org" file)))
                       (org-agenda-files))))
    (message "Getting done and IP tasks since %s" last-scrum-date)
    (apply 'append
           (mapcar
            (lambda (file)
              (message "Processing file: %s" file)
              (my/org-process-file-for-done-and-ip file last-scrum-date))
            agenda-files))))

;;(my/org-get-done-and-ip-tasks)

(defun my/org-process-file-for-next (file next-scrum-date)
  "Process a single file FILE for tasks scheduled until NEXT-SCRUM-DATE.
Returns a list of strings of tasks with links."
  (with-current-buffer (find-file-noselect file)
    (let ((parsed-buffer (org-element-parse-buffer)))
      (org-element-map parsed-buffer '(headline inlinetask)
        (lambda (element)
          (let* ((closed (org-element-property :closed element))
                 (closed-date (when closed
                                (org-time-string-to-absolute
                                 (my/org-timestamp-to-string closed))))
                 (scheduled (org-element-property :scheduled element))
                 (scheduled-date (when scheduled
                                   (org-time-string-to-absolute
                                    (my/org-timestamp-to-string scheduled))))
                 (begin (org-element-property :begin element))
                 (link (my/org-make-element-link element (buffer-file-name)))
                 (upcoming (and scheduled-date
                                (<= scheduled-date (org-time-string-to-absolute next-scrum-date))))
                 (in-progress (my/status-equals element "IP")))
            (when (or in-progress (and upcoming (not closed)))
              (let ((formatted-string (format "- [%s] %s" (if in-progress "-" " ") link)))
                (set-text-properties 0 (length formatted-string) nil formatted-string)
                formatted-string))))
        nil nil nil t))))

(defun my/org-get-next-tasks ()
  "Get tasks scheduled until the next scrum across all agenda files.
Returns a string of tasks with links."
  (let ((next-scrum-date (org-read-date nil nil (my/org-get-next-scrum-date)))
        (agenda-files (seq-filter
                       (lambda (file) (not (string-match-p "meds.org" file)))
                       (org-agenda-files))))
    (message "Getting next tasks until %s" next-scrum-date)
     (apply 'append
           (mapcar
            (lambda (file)
              (message "Processing file: %s" file)
              (my/org-process-file-for-next file next-scrum-date))
            agenda-files))))

;;(my/org-get-next-tasks)

(defun my/org-get-current-goals-from-journal ()
  "Retrieve the content under the '**** Current Goals' heading from the last entry in journal.org and remove all properties."
  (message "Retrieving Current Goals from journal.org")
  (with-current-buffer (find-file-noselect (my/org-get-journal-file-path))
    (goto-char (point-max))
    (when (re-search-backward "^\\*\\*\\*\\* Current Goals" nil t)
      (let ((start (progn (forward-line) (point))))
        (let ((end (or (re-search-forward "^\\*\\{1,4\\} " nil t) (point-max))))
          (let ((goals (buffer-substring-no-properties start (match-beginning 0))))
            (with-temp-buffer
              (insert goals)
              ;; Remove all properties from the buffer
              (goto-char (point-min))
              (while (re-search-forward "^:PROPERTIES:$" nil t)
                (let ((prop-start (match-beginning 0)))
                  (re-search-forward "^:END:$" nil t)
                  (delete-region prop-start (point))))
              (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun my/org-add-subsection-with-properties (title)
  "Add a new subsection with TITLE and :PROPERTIES: drawer containing :CREATED: property with the current date."
  (insert (format "%s\n" title))
  (insert ":PROPERTIES:\n")
  (insert (format ":CREATED:  [%s]\n" (format-time-string "%Y-%m-%d %a %H:%M")))
  (insert ":END:\n\n"))


(defun my/scrum ()
  "Update the Scrum section with done and next tasks."
  (interactive)
  (message "Updating Scrum section")
  (save-excursion
    (let ((scrum-headline-pos (org-find-exact-headline-in-buffer "Scrum"))
          (current-goals (my/org-get-current-goals-from-journal))
          (done-tasks (my/org-get-done-and-ip-tasks))
          (next-tasks (my/org-get-next-tasks)))
      
      (if scrum-headline-pos
          (progn
            (goto-char scrum-headline-pos)
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (outline-next-heading)
            (let ((start (point)))
              (goto-char (point-max))
              (if (re-search-backward "^\\*\\{2,\\} " nil t)
                  (goto-char (match-beginning 0))
                (goto-char (point-max)))
              (delete-region start (point)))
            (insert "\n")
            (my/org-add-subsection-with-properties "** Current Goals\n")
            (when current-goals
              (insert current-goals))
            (my/org-add-subsection-with-properties "** Since Last")
            (dolist (task done-tasks)
              (insert task)
              (insert "\n"))
            (my/org-add-subsection-with-properties "** Until Next")
            (dolist (task next-tasks)
              (insert task)
              (insert "\n"))
            (my/org-add-subsection-with-properties "** Impediments")
            (my/org-add-subsection-with-properties "** Notes")
            (widen))
        (message "Scrum headline not found"))))
  (message "Scrum section updated"))

(defun my/scrum-journal ()
  "Archive each subtree under the Scrum section in order: Current Goals, Since Last, Until Next, Impediments, Notes."
  (interactive)
  (message "Archiving subtrees under Scrum section to journal.org")
  (let ((org-archive-location (concat (my/org-get-journal-file-path) "::datetree/")))
    (save-excursion
      (let ((scrum-headline-pos (org-find-exact-headline-in-buffer "Scrum")))
        (if scrum-headline-pos
            (progn
              (goto-char scrum-headline-pos)
              (org-narrow-to-subtree)
              (goto-char (point-min)) 
              (outline-next-visible-heading 1)
              (while (not (eobp))
                (org-archive-subtree)
                (goto-char (point-min)) 
                (outline-next-visible-heading 1))
              (widen)
              (save-buffer)
              (message "Subtrees under Scrum section archived to journal.org"))
          (message "Scrum headline not found"))))))

(global-set-key (kbd "C-c o S") 'my/scrum)
(global-set-key (kbd "C-c o J") 'my/scrum-journal)

(provide 'mpemer_010_org)
;;; mpemer_010_org.el ends here
