(setq package-list
       '(ace-window
 	cider
 	markdown-mode
 	groovy-mode
	haskell-mode
 	magit
 	neotree
	edit-server

	wgrep
	company
	
	org-mime
	org-plus-contrib
	org-alert
	org-bullets
	org-ac
	org-jira
	org-pdfview

	oauth2
	
	pandoc
	pandoc-mode
	ox-pandoc

	slime
	
	symon

	confluence
	
	copy-as-format

	yaml-mode
	
	org-beautify-theme
	gruber-darker-theme

;;	bbdb ;;manually installed with specific version
	))

(setq package-user-dir "~/.emacs.d/elpa"
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(package-initialize)
(unless (file-exists-p package-user-dir) (package-refresh-contents))
(dolist (package package-list) (unless (package-installed-p package) (package-install package)))

;;(require 'bbdb)

