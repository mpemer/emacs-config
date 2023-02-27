;;; Package --- Summary:

;;; Commentary:

;;; Code:


;; ORG-MODE CONFIG CHANGES

;;; Code:
(require 'defs)

(setq org-directory (my/mkpath my/home "org"))

(defun mp-org-notes ()
  "Open notes.org."
  (interactive)
  (find-file (concat org-directory "/notes.org")))
(defun mp-org-plan ()
  "Open plan.org."
  (interactive)
  (find-file (concat org-directory "/plan.org")))
(defun mp-org-mercury ()
  "Open mercury.org."
  (interactive)
  (find-file "~/org/mercury.org"))
(defun mp-org-bookmarks ()
  "Open bookmarks.org."
  (interactive)
  (find-file (concat org-directory "/bookmarks.org")))

(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c om") 'mp-org-mercury)
(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)


;; By default, archive into the same file name under archive subfolder, but fold items into datetree
(setq org-archive-location (my/mkpath "archive" "%s::datetree"))

(defun my/org-home (filename)
  "Concat whatever FILENAME to the org-home path."
  (concat "~/org/" filename))


(setq org-directory "~/org"
      org-agenda-files (list "~/org")
      org-caldav-save-directory "~/org/"
      org-default-notes-file "~/org/notes.org"
      org-icalendar-timezone "Europe/Wien"
      plstore-cache-passphrase-for-symmetric-encryption t

      org-caldav-sync-todo nil
      org-icalendar-include-todo nil
      org-icalendar-use-deadline '(event-if-not-todo event-if-todo)
      org-icalendar-use-scheduled '(event-if-not-todo event-if-todo)

      org-caldav-debug-level 0

      )


;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ;;("@errand" . ?e)
                            ;;("@office" . ?o)
                            ;;("@home" . ?H)
			                      ;;("iteego"   . ?i)
			                      ;;("kohler"   . ?k)
                            ("pemer"    . ?p)
                            ("personal" . ?f)
                            ("flag"     . ??)
                            (:endgroup))))


(defun my/org-caldav-sync ()
  "Syncing org-caldav with async."
  (interactive)
  (org-caldav-sync)
  (let ((old-buffer (current-buffer)))
    (dolist (b '("notes.org" "plan.org" "family.org"))
      (switch-to-buffer b)
      (when (buffer-modified-p) (save-buffer)))
    (switch-to-buffer old-buffer)))

(global-set-key "\C-cs" 'my/org-caldav-sync)

(setq org-feed-alist
      '(("Slashdot"
         "http://rss.slashdot.org/Slashdot/slashdot"
         "~/org/feeds.org" "Slashdot")
        ("NYT"
         "http://rss.art19.com/the-daily"
         "~/org/feeds.org" "NYT")))

(provide 'mpemer_010_org)
;;; mpemer_010_org.el ends here
