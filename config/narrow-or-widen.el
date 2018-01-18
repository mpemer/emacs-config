 (defun mp/narrow-or-widen-dwim (p)
    "Works like distraction-free mode toggle. If the buffer is 
narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
	  ((region-active-p)
	   (narrow-to-region (region-beginning) (region-end)))
	  ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
	  (t (narrow-to-defun))))

(global-set-key (kbd "C-x =") 'mp/narrow-or-widen-dwim)
