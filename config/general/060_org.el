;;; Package --- Summary:
;; Org mode configuration for Emacs

;;; Commentary:

;;; Code:

(require 'mydefs)

(add-to-list 'load-path (my/mkpath user-emacs-directory "org-mode" "lisp"))
(add-to-list 'load-path (my/mkpath user-emacs-directory "org-contrib" "lisp"))

(require 'org-agenda)


(let ((package-list '(org-alert
		                  ;;org-beautify-theme
		                  org-ehtml
		                  org-jira
		                  ;;org-pdfview
		                  )))
  (dolist (package package-list)
    (progn
			(my/ensure-package-installed package)
			(use-package package))))

(let ((package-list '(ox-pandoc ;; https://github.com/kawabata/ox-pandoc
		                  ox-asciidoc
		                  ox-clip
		                  ox-epub
		                  ox-jira
		                  ox-minutes
		                  ox-slack
		                  ox-twbs
		                  ox-odt ;; https://github.com/kjambunathan/org-mode-ox-odt/blob/master/README.md
		                  ox-hugo)))
  (dolist (package package-list)
    (progn
			(my/ensure-package-installed package)
			(use-package package
				:ensure t
				:after ox))))

(progn
  (require 'org-num)
  (add-hook 'org-mode-hook #'org-num-mode))


;; ob-clojure
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
(progn
  (use-package ob-clojure
    :config (progn
	      (setq org-babel-clojure-backend 'cider))))

(progn
  (use-package ob-clojure)
  (use-package ob-emacs-lisp)
  (use-package ob-java)
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

(org-babel-do-load-languages
 'org-babel-load-languages '(
			     (clojure . t)
			     (emacs-lisp . t)
			     (java . t)
			     (lisp . t)
			     (org . t)
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


;; If I wanted to remove the above and instead use css styling for exported code blocks
(setq org-html-htmlize-output-type 'css)
;;(setq org-html-htmlize-output-type 'inline-css)

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




(defun my/org-print-wrap-hook (exporter)
  (when (eq exporter 'html)
    "@media print {
       pre {
         white-space: pre-wrap;
       }
      }"))

(add-hook 'org-export-before-processing-hook 'my/org-print-wrap-hook)


(global-set-key (kbd "C-c C-.") 'org-insert-structure-template)


(setq org-lowest-priority ?F
      org-default-priority ?B)


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

(defun todo-to-int (todo)
  "Convert TODO item to int value, for sorting."
    (first (-non-nil
            (mapcar (lambda (keywords)
                      (let ((todo-seq
                             (-map (lambda (x) (first (split-string  x "(")))
                                   (rest keywords))))
                        (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                    org-todo-keywords))))

(defun my/org-sort-key ()
  "Assign an integer sort value to org entries based on type, priority and date."
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
  "Sort ORG entries according to my rules."
  (interactive)
  (org-sort-entries nil ?f #'my/org-sort-key))

(global-set-key (kbd "C-c os") 'my/org-sort-entries)

(setq org-capture-templates
      (quote (("t" "TODO" entry (file+headline "~/org/notes.org" "Tasks")
	       "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nDEADLINE: %t\nSCHEDULED: \n")
	      ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
	       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	      ("m" "Meeting" entry (file+headlines "~/org/notes.org" "Meetings")
	       "* Meeting with %?\n:PROPERTIES:\n:CREATED: %U\n:SCHEDULED: %t\n:END:\n"))))

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

;; Adds a CLOSED property time stamp to the TODO entry if marked as done
(setq org-log-done 'time)

;;;; END REFILE
(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.

If a is before b, return -1. If a is after b, return 1. If they
are equal return nil."
  (lexical-let ((prop prop))
  #'(lambda (a b)

    (let* ((a-pos (get-text-property 0 'org-marker a))
           (b-pos (get-text-property 0 'org-marker b))
           (a-date (or (org-entry-get a-pos prop)
                       (format "<%s>" (org-read-date t nil "now"))))
           (b-date (or (org-entry-get b-pos prop)
                       (format "<%s>" (org-read-date t nil "now"))))
           (cmp (compare-strings a-date nil nil b-date nil nil))
           )
      (if (eq cmp t) nil (signum cmp))
      ))))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "PLAN(p)" "IP(i)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING" "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light gray" :weight bold)
              ("PLAN" :foreground "light blue" :weight bold)
              ("IP" :foreground "light blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
	      ("EVENT" :foreground "light blue" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change t)

;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING") ("HOLD" . t))
;;               (done ("WAITING") ("HOLD"))
;;               ("TODO" ("WAITING") ("IP") ("CANCELLED") ("HOLD"))
;;               ("PLAN" ("WAITING") ("IP")("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("IP")("CANCELLED") ("HOLD")))))


(defun elisp-showdoc (f)
  "Show docstring for F in notification area."
  (interactive (list (thing-at-point 'symbol t)))
  (message
   "%s"
   (let* ((doc-list      (split-string (documentation (intern f)) "\n"))
          (number-lines  (min (- (floor (* max-mini-window-height (frame-height))) 2)
                              (- (length doc-list) 2)))
          (subset        (concatenate 'list
                                      (last doc-list)
                                      '("")
                                      (subseq doc-list 0 number-lines)))
          (pruned-subset (if (string-equal (car (last subset)) "")
                             (butlast subset)
                             subset)))
     (mapconcat #'identity pruned-subset "\n"))))
;;;; AGENDA

;; Place tags close to the right-hand side of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

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
;;			     (load-theme 'org-beautify))))

;; By default, save openoffice exports as ms word documents
(setq org-export-odt-preferred-output-format "docx")
(setq org-odt-preferred-output-format "docx")

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


;; (add-hook 'focus-in-hook 
;;   (lambda () (progn 
;;     (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

;; (add-hook 'focus-out-hook 
;;   (lambda () (progn 
;;     (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))


;;
;; Add view to agenda that lists past two weeks
;;
;; (add-to-list 'org-agenda-custom-commands

;; active Babel languages
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((sh . t)
;;   (clojure . t)
;;   (emacs-lisp . t)))
(provide '060_org)
;;; 060_org.el ends here
