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
(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)


;; By default, archive into the same file name under archive subfolder, but fold items into datetree
(setq org-archive-location (my/mkpath "archive" "%s::datetree"))

(setq org-directory (expand-file-name "~/org")
      org-agenda-files (append (list (expand-file-name "tasks.org" org-directory)
                                     (expand-file-name "plan.org" org-directory)
                                     (expand-file-name "goals.org" org-directory))
                               (directory-files-recursively
                                (expand-file-name "calendars" org-directory) "\\.org$"))
      org-caldav-save-directory (expand-file-name ".caldav" org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-icalendar-timezone "CET"
      plstore-cache-passphrase-for-symmetric-encryption t

      org-caldav-sync-todo nil
      org-icalendar-include-todo nil
      org-icalendar-use-deadline '(event-if-not-todo event-if-todo)
      org-icalendar-use-scheduled '(event-if-not-todo event-if-todo)

      org-caldav-debug-level 0

      )

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

(provide 'mpemer_010_org)
;;; mpemer_010_org.el ends here
