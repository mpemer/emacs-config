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


;; (progn
;;   (ensure-package-installed 'org-gcal)
;;   (use-package org-gcal
;;     :config (progn
;; 	     (setq org-gcal-client-id "441016108337-1hupr92oqr0kbk71uiuhe377ji6n6pqm.apps.googleusercontent.com"
;; 		   org-gcal-client-secret "GdpE7SUxUWXbMiOG9USuIpsA"
;; 		   org-gcal-file-alist '(("pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com" .  "~/org/plan.org"))
;; 		   org-gcal-header-alist '(("441016108337-1hupr92oqr0kbk71uiuhe377ji6n6pqm.apps.googleusercontent.com" . "#+PROPERTY: TIMELINE_FACE \"pink\"\n"))
;; 		   org-gcal-auto-archive nil
;; 		   org-gcal-notify-p nil)
;; 	     (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
;; 	     (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch))))


(progn
  (require 'org-num)
  (add-hook 'org-mode-hook #'org-num-mode))

(progn
  (ensure-package-installed 'org-fancy-priorities)
  (use-package org-fancy-priorities
    :config (progn
	      (unless (char-displayable-p ?❗)
		(setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))
		(add-hook 'org-mode-hook #'org-fancy-priorities-mode))))

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
    
(global-set-key (kbd "C-c on") 'mp-org-notes)
(global-set-key (kbd "C-c op") 'mp-org-plan)
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
                            ("waiting" . ?w)
                            ("ip" . ?p)
			    ("iteego" . ?i)
			    ("kohler" . ?k)
			    ("mrmaster" . ?m)
                            ("hold" . ?h)
                            ("personal" . ?P)
                            ("work" . ?W)
                            ("org" . ?O)
                            ("crypt" . ?E)
                            ("note" . ?n)
                            ("cancel" . ?c)
                            ("flag" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(with-eval-after-load 'org       
  (setq org-startup-indented t ; Enable `org-indent-mode' by default
	org-src-tab-acts-natively t)

  (add-hook 'org-mode-hook (lambda ()
			     visual-line-mode
			     flyspell-mode)))

;; By default, save openoffice exports as ms word documents
(setq org-export-odt-preferred-output-format "docx")
(setq org-odt-preferred-output-format "docx")
