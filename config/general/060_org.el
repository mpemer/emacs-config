;;; Package --- Summary:
;; Org mode configuration for Emacs

;;; Commentary:

;;; Code:

(require 'defs)

;;(add-to-list 'load-path (my/mkpath user-emacs-directory "org-mode" "lisp"))
;;(add-to-list 'load-path (my/mkpath user-emacs-directory "org-contrib" "lisp"))

(require 'org-agenda)


(let ((package-list '(org-alert   ;; https://github.com/spegoraro/org-alert
		                  org-bullets ;; https://github.com/sabof/org-bullets
                      org-caldav  ;; https://github.com/dengste/org-caldav
                      async       ;; https://github.com/jwiegley/emacs-async
                      org-contrib
                      org-beautify-theme
		                  )))
  (dolist (package package-list)
    (progn
      (my/ensure-package-installed package)
      (use-package package))))

;;(my/ensure-package-installed package)
;;(require 'org-contrib)
(let ((package-list '(ox-pandoc   ;; https://github.com/kawabata/ox-pandoc
		                  ox-epub     ;; https://github.com/ofosos/ox-epub
		                  ox-odt      ;; https://github.com/kjambunathan/org-mode-ox-odt/blob/master/README.md
		                  ox-hugo     ;; https://ox-hugo.scripter.co/
                      )))
  (dolist (package package-list)
    (progn
			(my/ensure-package-installed package)
			(use-package package
				:ensure t
				:after ox))))

(with-eval-after-load 'ox
  (require 'ox-hugo))

(use-package ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package ox-latex
    :config
    ;; code here will run after the package is loaded
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)

    ;; deleted unwanted file extensions after latexMK
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil)))

(use-package org-beautify-theme)
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ob-clojure
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
;;(progn
;;  (use-package ob-clojure
;;    :config (progn
;;	      (setq org-babel-clojure-backend 'cider))))

(progn
  ;;(use-package ob-clojure)
  (use-package ob-emacs-lisp)
  ;;(use-package ob-java)
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

(setq org-export-with-toc nil)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c C-b") 'org-switchb)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "C-c c") 'org-capture)


;; Shortcut to inserting structured templates, like code blocks (list below)
(global-set-key (kbd "C-c C-.") 'org-insert-structure-template)
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


;; Keep deep levels of priorities
(setq org-lowest-priority ?F
      org-default-priority ?B)


;;;; REFILE

;; Targets include this file and any file contributing to the agenda - up to 3 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))


;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)


(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)
;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Adds a CLOSED property time stamp to the TODO entry if marked as done
(setq org-log-done 'time)

;;;; END REFILE

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "FILE(f)" "IP(i)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
              (sequence "MEETING(m)" "EVENT(e)"))))

(setq org-caldav-todo-percent-states
      (quote ((0 "TODO") (0 "FILE") (50 "IP") (100 "DONE")   (:calendar-id "marcus@pemer.com"
                 :files ("~/org/tasks.org")
                 :inbox "~/org/tasks.org"
                 )

              (0 "WAITING") (0 "HOLD") (0 "CANCELLED")
              (0 "MEETING") (0 "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light gray" :weight bold)
              ("FILE" :foreground "light blue" :weight bold)
              ("IP" :foreground "light blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
	            ("EVENT" :foreground "light blue" :weight bold))))

;; Change item state with C-c C-t
(setq org-use-fast-todo-selection 'expert)

;; Change item state with Shift-[left|right]
(setq org-treat-S-cursor-todo-selection-as-state-change t)

;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING") ("HOLD" . t))
;;               (done ("WAITING") ("HOLD"))
;;               ("TODO" ("WAITING") ("IP") ("CANCELLED") ("HOLD"))
;;               ("PLAN" ("WAITING") ("IP")("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("IP")("CANCELLED") ("HOLD")))))


;; (defun elisp-showdoc (f)
;;   "Show docstring for F in notification area."
;;   (interactive (list (thing-at-point 'symbol t)))
;;   (message
;;    "%s"
;;    (let* ((doc-list      (split-string (documentation (intern f)) "\n"))
;;           (number-lines  (min (- (floor (* max-mini-window-height (frame-height))) 2)
;;                               (- (length doc-list) 2)))
;;           (subset        (concatenate 'list
;;                                       (last doc-list)
;;                                       '("")
;;                                       (subseq doc-list 0 number-lines)))
;;           (pruned-subset (if (string-equal (car (last subset)) "")
;;                              (butlast subset)
;;                              subset)))
;;      (mapconcat #'identity pruned-subset "\n"))))
;;;; AGENDA

;; Place tags close to the right-hand side of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

;;;; Do not dim blocked tasks
;;(setq org-agenda-dim-blocked-tasks nil)

;;;; Compact the block agenda view
;;(setq org-agenda-compact-blocks t)

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

(setq org-id-search-archives nil)

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


;;(setq org-enable-hugo-support

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



;;
;; Inject logic that adds .org extenions to path of org file links that have no suffix,
;; but where the target file exists
;;
(defun org-link-open-as-file (path arg)
  "Pretend PATH is a file name and open it.

According to \"file\"-link syntax, PATH may include additional
search options, separated from the file name with \"::\".

This function is meant to be used as a possible tool for
`:follow' property in `org-link-parameters'."
  (let* ((option (and (string-match "::\\(.*\\)\\'" path)
		                  (match-string 1 path)))
	       (file-name (if (not option) path
		                  (substring path 0 (match-beginning 0))))
         (org-file-name (concat (string-trim-right file-name "\\.") ".org"))
         (alt-file-name (if (and (= 0 (length (file-name-extension file-name)))
                                 (not (file-exists-p file-name)))
;;                                 (file-exists-p org-file-name))
                            org-file-name
                          file-name)))

    (if (string-match "[*?{]" (file-name-nondirectory file-name))
	      (dired file-name)
      (apply #'org-open-file
	     alt-file-name
	     arg
	     (cond ((not option) nil)
		         ((string-match-p "\\`[0-9]+\\'" option)
		          (list (string-to-number option)))
		         (t (list nil option)))))))



(provide '060_org)
;;; 060_org.el ends here
