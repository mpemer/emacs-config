(progn
  (ensure-package-installed 'jira-markup-mode)
  (use-package jira-markup-mode
    :config (progn
	      (add-to-list 'auto-mode-alist '(".*atlassian.*\\.txt$" . jira-markup-mode))
	      (add-to-list 'auto-mode-alist '(".*iteego.jira.*\\.txt$" . jira-markup-mode)))))

