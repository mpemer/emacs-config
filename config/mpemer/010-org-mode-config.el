;; ORG-MODE CONFIG CHANGES

;; First make sure the packages are installed
(let ((package-list '(
		      org-alert
		      ;;org-beautify-theme		      
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


(setq org-gcal-file-alist '(("pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" .  "~/org/plan.org")))

(progn
  (require 'org-num)
  (add-hook 'org-mode-hook #'org-num-mode))

;; (progn
;;   (ensure-package-installed 'org-fancy-priorities)
;;   (use-package org-fancy-priorities
;;     :config (progn
;; 	      (unless (char-displayable-p ?❗)
;; 		(setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))
;; 		(add-hook 'org-mode-hook #'org-fancy-priorities-mode))))

;; ob-clojure
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
(progn
;;  (ensure-package-installed 'ob-clojure)
  (use-package ob-clojure
    :config (progn
	      (setq org-babel-clojure-backend 'cider))))

(progn
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

(setq org-export-with-toc nil)

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

(setq org-lowest-priority ?F
      org-default-priority ?B)

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
(defun mp-org-iteego ()
  (interactive)
  (find-file "~/iteego/org/iteego.org"))
(defun mp-org-kohler ()
  (interactive)
  (find-file "~/kohler/org/kohler.org"))
(defun mp-org-mercury ()
  (interactive)
  (find-file "~/mercury/org/mercury.org"))
(defun mp-org-bookmarks ()
  (interactive)
  (find-file (concat org-directory "/bookmarks.org")))
(defun mp-emacs ()
  (interactive)
  (find-file "~/.emacs.d/config/.emacs"))
    
(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c op") 'mp-org-plan)
(global-set-key (kbd "C-c oi") 'mp-org-iteego)
(global-set-key (kbd "C-c ok") 'mp-org-kohler)
(global-set-key (kbd "C-c om") 'mp-org-mercury)
(global-set-key (kbd "C-c ob") 'mp-org-bookmarks)

(defun my/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'my/org-sort-key))

;; By default, archive into the same file name under archive subfolder, but fold items into datetree
(setq org-archive-location (concat "archive/%s::datetree/"))

(defun my/org-home (filename)
  (concat "~/org/" filename))

(setq prj-folders '("pemer" "mercury" "iteego" "kohler" "mrmaster" "personal"))

(setq org-directory "~/org"
      org-agenda-files (cons "~/org" (mapcar (lambda (folder) (concat "~/" folder "/org")) prj-folders))
      org-default-notes-file "~/org/notes.org"
      org-icalendad-timezone "Europe/Wien")

(setq org-capture-templates
      (quote (("t" "TODO" entry (file+headline "~/org/notes.org" "Tasks")
	       "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nDEADLINE: %t\nSCHEDULED: \n")
	      ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
	       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	      ("m" "Meeting" entry (file+headlines "~/org/notes.org" "Meetings")
	       "* Meeting with %?\n:PROPERTIES:\n:CREATED: %U\n:SCHEDULED: %t\n:END:\n"))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c C-b") 'org-switchb)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)

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

;;;; END REFILE

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "IP(p)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING" "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light gray" :weight bold)
              ("NEXT" :foreground "light blue" :weight bold)
              ("IP" :foreground "light blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
	      ("EVENT" :foreground "light blue" :weight bold))))

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

;; Make it so that Agenda view stops warning about deadlines once item has been scheduled
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (org-save-all-org-buffers)))

;; (add-hook 'focus-in-hook 
;;   (lambda () (progn 
;;     (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

;; (add-hook 'focus-out-hook 
;;   (lambda () (progn 
;;     (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))


(setq org-crypt-key "1D151FF890EE620251BC79A4E594D6C2CC9E1BAA")
;;

;; (if (file-exists-p "~/src/org-caldav/org-caldav.el")
;;     (setq org-caldav-oauth2-client-id "931083343613-1u4fv2nb4tf1ktgq2678it8o3jtj3qlq.apps.googleusercontent.com"
;; 	  org-caldav-oauth2-client-secret "4TDz6ByKh341Ik3mzHU7wAlB"
;; 	  org-caldav-calendar-id "pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com"
;; 	  org-caldav-calendars '((:calendar-id "pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" :files ("~/org/plan.org")))))


;; (add-to-list 'load-path "/home/mpemer/src/org-gcal.el")
;; (require 'org-gcal)

;; ;;(progn
;; ;;  (ensure-package-installed 'org-gcal)
;; ;;  (use-package org-gcal
;; ;;    :config
;;     (setq org-gcal-client-id "441016108337-1hupr92oqr0kbk71uiuhe377ji6n6pqm.apps.googleusercontent.com"
;; 		  org-gcal-client-secret "GdpE7SUxUWXbMiOG9USuIpsA"
;; 		  org-gcal-file-alist '(("pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" .  "~/org/plan.org"))
;; 		  org-gcal-up-days 30
;; 		  org-gcal-down-days 365)
;; ;;))

;; (global-set-key (kbd "C-c oS") 'org-gcal-sync)
;; (global-set-key (kbd "C-c od") 'org-gcal-delete-at-point)


(add-to-list 'load-path "~/src/org-caldav")
(require 'org-caldav)
(progn
	     (setq plstore-cache-passphrase-for-symmetric-encryption t
		   org-caldav-url 'google
		   org-caldav-calendar-id "pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com"
		   org-caldav-inbox "~/org/plan.org"
		   org-caldav-files '("~/org/plan.org")
		   org-caldav-debug-level 2
		   org-icalendar-timezone "UTC"
		   org-caldav-oauth2-client-id "931083343613-n93s4de581lin78uknno7fs15pkmevl1.apps.googleusercontent.com"
		   org-caldav-oauth2-client-secret "zuDwQKI-9b0g9-tidkgcYqsa"
		   org-caldav-save-directory "~/org/org-caldav-state.el"
		   org-caldav-debug-level 2)
 	     (global-set-key (kbd "C-c oS") 'org-caldav-sync))
(setq plstore-cache-passphrase-for-symmetric-encryption t)




(require 'org-gcal)
(setq org-gcal-client-id "931083343613-n93s4de581lin78uknno7fs15pkmevl1.apps.googleusercontent.com"
      org-gcal-client-secret "zuDwQKI-9b0g9-tidkgcYqsa"
      org-gcal-file-alist '(("pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" .  "~/org/plan.org")))

