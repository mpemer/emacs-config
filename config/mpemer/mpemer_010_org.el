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
  (find-file (concat org-directory "/tasks.org")))

(defun mp-org-notes ()
  "Open notes.org."
  (interactive)
  (find-file (concat org-directory "/notes.org")))

(defun mp-org-meetings ()
  "Open meetings.org."
  (interactive)
  (find-file (concat org-directory "/meetings.org")))

(defun mp-org-plan ()
  "Open plan.org."
  (interactive)
  (find-file (concat org-directory "/plan.org")))

(defun mp-org-goals ()
  "Open goals.org."
  (interactive)
  (find-file (concat org-directory "/goals.org")))

(defun mp-org-ideas ()
  "Open ideas.org."
  (interactive)
  (find-file (concat org-directory "/ideas.org")))

(defun mp-org-family ()
  "Open family.org."
  (interactive)
  (find-file (concat org-directory "/calendars/family.org")))

(defun mp-org-bookmarks ()
  "Open bookmarks.org."
  (interactive)
  (find-file (concat org-directory "/bookmarks.org")))

(global-set-key (kbd "C-c oo") 'mp-org-tasks)
(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c om") 'mp-org-meetings)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c og") 'mp-org-goals)
(global-set-key (kbd "C-c oi") 'mp-org-ideas)
(global-set-key (kbd "C-c of") 'mp-org-family)
(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)


;; By default, archive into the same file name under archive subfolder, but fold items into datetree
(setq org-archive-location (my/mkpath "archive" "%s::datetree"))

(defun my/org-home (filename)
  "Concat whatever FILENAME to the org-home path."
  (concat "~/org/" filename))


(setq org-directory "~/org"
      org-agenda-files (list "~/org")
      org-caldav-save-directory "~/org/.caldav/"
      org-default-notes-file "~/org/notes.org"
      org-icalendar-timezone "CET"
      plstore-cache-passphrase-for-symmetric-encryption t

      org-caldav-sync-todo nil
      org-icalendar-include-todo nil
      org-icalendar-use-deadline '(event-if-not-todo event-if-todo)
      org-icalendar-use-scheduled '(event-if-not-todo event-if-todo)

      org-caldav-debug-level 0

      )

(setq org-capture-templates
      (quote (("t" "Task" entry (file "~/org/tasks.org")
 	             "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nDEADLINE: %t\nSCHEDULED: \n")
 	            ("n" "Note" entry (file "~/org/notes.org")
 	             "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
 	            ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
 	             "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
 	            ("g" "Goal" entry (file "~/org/goals.org")
 	             "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
 	            ("m" "Meeting" entry (file "~/org/meetings.org")
 	             "* MEETING with %?\n:PROPERTIES:\n:CREATED: %U\n:SCHEDULED: %t\n:END:\n")
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



(setq org-feed-alist
      '(("Slashdot"
         "http://rss.slashdot.org/Slashdot/slashdot"
         "~/org/feeds.org" "Slashdot")))
;;        ("NYT"
;;         "http://rss.art19.com/the-daily"
;;         "~/org/feeds.org" "NYT")))

(provide 'mpemer_010_org)
;;; mpemer_010_org.el ends here
