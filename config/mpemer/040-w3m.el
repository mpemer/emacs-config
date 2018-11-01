(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(global-set-key "\C-xw" 'w3m-goto-url)


(setq wicked/quick-search-alist
      '(("^g?:? +\\(.*\\)" . ;; Google Web 
         "http://www.google.com/search?q=\\1")
	
        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
	
	("^dict:? +\\(.*\\)" . ;; Dictionary
	 "http://dictionary.reference.com/search?q=\\1")))

(require 'cl-seq)
(defadvice w3m-goto-url (before wicked activate)
  "Use the quick searches defined in `wicked/quick-search-alist'."
  (let* ((my-url (replace-regexp-in-string 
		  "^ *\\| *$" "" 
		  (replace-regexp-in-string "[ \t\n]+" " " (ad-get-arg 0))))
	 (match (assoc-if
		 (lambda (a) (string-match a my-url))
		 wicked/quick-search-alist)))
    (if match
	(ad-set-arg 0 (replace-regexp-in-string
		       (car match) (cdr match) my-url)))))

(defadvice browse-url (before wicked activate)
  "Use the quick searches defined in `wicked/quick-search-alist'."
  (let* ((my-url (replace-regexp-in-string 
		  "^ *\\| *$" "" 
		  (replace-regexp-in-string "[ \t\n]+" " " (ad-get-arg 0))))
	 (match (assoc-if
		 (lambda (a) (string-match a my-url))
		 wicked/quick-search-alist)))
    (if match
	(ad-set-arg 0 (replace-regexp-in-string
		       (car match) (cdr match) my-url)))))

