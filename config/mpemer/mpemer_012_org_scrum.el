;;; -*- mode: emacs-lisp  -*-
;; Package --- Summary:

;;; Commentary:

;;; Code:


;; ORG-MODE SCRUM LOGIC

;;; Code:
(require 'defs)
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
    (if id ;; if we already have an ID defined - use that
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
              ;; Remove leading and trailing whitespace
              (goto-char (point-min))
              (while (re-search-forward "\\`[ \t\n\r]+" nil t)
                (replace-match ""))
              (goto-char (point-max))
              (while (re-search-backward "[ \t\n\r]+\\'" nil t)
                (replace-match ""))
              (buffer-substring-no-properties (point-min) (point-max)))))))))


(defun my/org-insert-created ()
  "Insert :PROPERTIES: drawer with :CREATED: property containing the current date."
  (insert ":PROPERTIES:\n")
  (insert (format ":CREATED:  [%s]\n" (format-time-string "%Y-%m-%d %a %H:%M")))
  (insert ":END:\n\n"))


(defun my/org-ensure-subsection (title &optional &key created)
  "Ensure a subsection with TITLE exists. Optionally add :PROPERTIES: drawer with :CREATED: property if CREATED is t.
If the subsection does not exist, it is created."
  (let ((heading-exists (save-excursion
                          (goto-char (point-min))
                          (org-find-exact-headline-in-buffer title))))
    (unless heading-exists
      (insert (format "%s\n" title)))
    (when created
     (let ((has-properties (save-excursion
                             (org-back-to-heading t)
                             (re-search-forward "^:PROPERTIES:" (save-excursion (outline-next-heading) (point)) t))))
       (unless has-properties
         (org-back-to-heading t)
         (outline-next-heading)
         (forward-line -1)
         (my/org-insert-created))))))


(defun my/org-ensure-created-in-subheadings ()
  "Ensure all subheadings have a :PROPERTIES: drawer with :CREATED: property containing the current date."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((has-properties (save-excursion
                             (org-back-to-heading t)
                             (re-search-forward "^:PROPERTIES:" (save-excursion (outline-next-heading) (point)) t))))
       (unless has-properties
         (org-back-to-heading t)
         (outline-next-heading)
         (forward-line -1)
         (my/org-insert-created))))
   t 'tree))


(defun my/scrum-create ()
  "Create a new Scrum section with current goals."
  (interactive)
  (message "Updating Scrum section")
  (save-excursion
    (let ((scrum-headline-pos (org-find-exact-headline-in-buffer "Scrum"))
          (current-goals (my/org-get-current-goals-from-journal)))
      
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
            (my/org-ensure-subsection "** Current Goals\n" :created t)
            (when current-goals
              (insert current-goals "\n\n"))
            (dolist (title '("** Impediments\n"
                             "** Notes\n"))
              (my/org-ensure-subsection title))
            (widen))
        (message "Scrum headline not found"))))
  (message "Scrum section updated"))


(defun my/scrum-report ()
  "Complete the Scrum section with done and next tasks."
  (interactive)
  (message "Updating Scrum section")
  (save-excursion
    (let ((scrum-headline-pos (org-find-exact-headline-in-buffer "Scrum"))
          (done-tasks (my/org-get-done-and-ip-tasks))
          (next-tasks (my/org-get-next-tasks)))
      
      (if scrum-headline-pos
          (progn
            (goto-char scrum-headline-pos)
            (org-narrow-to-subtree)
            (goto-char (point-min))

            ;; Search for and place point just before the "** Impediments" heading
            (if (re-search-forward "^\\*\\* Impediments" nil t)
                (goto-char (match-beginning 0))
              (goto-char (point-max)))

            (my/org-ensure-subsection "** Since Last" :created t)
            (dolist (task done-tasks)
              (insert task "\n"))
            (my/org-ensure-subsection "** Until Next" :created t)
            (dolist (task next-tasks)
              (insert task "\n"))
            (widen))
        (message "Scrum headline not found"))))
  (message "Scrum section updated"))


(defun my/scrum-commit ()
  "Journal each subtree under the Scrum section in order: Current Goals, Since Last, Until Next, Impediments, Notes."
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


(progn
  (global-set-key (kbd "C-c o S") 'my/scrum-create)
  (global-set-key (kbd "C-c o R") 'my/scrum-report)
  (global-set-key (kbd "C-c o C") 'my/scrum-commit))


(provide 'mpemer_012_org_scrum)
;;; mpemer_012_org_scrum.el ends here
