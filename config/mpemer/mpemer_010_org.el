;;; Package --- Summary:

;;; Commentary:

;;; Code:


;; ORG-MODE CONFIG CHANGES

;;; Code:

(defun my/reformat-for-scrum-notes ()
  "Reformat org to confluence scrum notes."
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
  "Convert region to scrum notes format."
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

(setq org-directory "~/org")

(defun mp-org-notes ()
  "Open notes.org."
  (interactive)
  (find-file (concat org-directory "/notes.org")))
(defun mp-org-plan ()
  "Open plan.org."
  (interactive)
  (find-file (concat org-directory "/plan.org")))
(defun mp-org-iteego ()
  "Open iteego.org."
  (interactive)
  (find-file "~/iteego/org/iteego.org"))
(defun mp-org-kohler ()
  "Open kohler.org."
  (interactive)
  (find-file "~/kohler/org/kohler.org"))
(defun mp-org-mercury ()
  "Open mercury.org."
  (interactive)
  (find-file "~/mercury/org/mercury.org"))
(defun mp-org-bookmarks ()
  "Open bookmarks.org."
  (interactive)
  (find-file (concat org-directory "/bookmarks.org")))
(defun mp-emacs ()
  "Open .emacs."
  (interactive)
  (find-file "~/.emacs.d/config/.emacs"))

(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c oi") 'mp-org-iteego)
(global-set-key (kbd "C-c ok") 'mp-org-kohler)
(global-set-key (kbd "C-c om") 'mp-org-mercury)
(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)

;; By default, archive into the same file name under archive subfolder, but fold items into datetree
(setq org-archive-location (concat "archive/%s::datetree/"))

(defun my/org-home (filename)
  "Concat whatever FILENAME to the org-home path."
  (concat "~/org/" filename))

(setq prj-folders '("pemer" "mercury" "iteego" "kohler" "mrmaster" "personal"))

(setq org-directory "~/org"
      org-agenda-files (cons "~/org" (mapcar (lambda (folder) (concat "~/" folder "/org")) prj-folders))
      org-default-notes-file "~/org/notes.org"
      org-icalendad-timezone "Europe/Wien")


                                        ; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ;;("@errand" . ?e)
                            ;;("@office" . ?o)
                            ;;("@home" . ?H)
                            
                            ("waiting" . ?w)
                            ("ip" . ?p)
                            ("crypt" . ?c)
                            ("hold" . ?h)
                            ("note" . ?n)
			                      ("iteego" . ?I)
			                      ("kohler" . ?K)
			                      ("mrmaster" . ?M)
                            ("personal" . ?P)
                            ("ratum" . ?R)
                            ("org" . ?O)
                            ("cancel" . ?C)
                            ("flag" . ??)
			                      (:endgroup))))


(setq org-crypt-key "1D151FF890EE620251BC79A4E594D6C2CC9E1BAA")

(setq org-feed-alist
      '(("Slashdot"
         "http://rss.slashdot.org/Slashdot/slashdot"
         "~/txt/org/feeds.org" "Slashdot Entries")))

(provide 'mpemer_010_org)
;;; mpemer_010_org.el ends here