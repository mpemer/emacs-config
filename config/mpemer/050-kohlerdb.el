(ensure-package-installed 'ido) (use-package ido)

(defun my/kohler-db ()
  "Prompt user to pick a kohler db connection from a list, then connect to it."
  (interactive)

  (flet ((orcl (ip port sid)
	       (format "(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(Host=%s)(Port=%s))(CONNECT_DATA=(SID=%s)))"
		       ip port sid)))

    ;; Define our db instances as tnsnames.ora connector strings
    (let ((atgdev   (orcl "10.94.132.146" "9101" "atgdev"))
	  (atgqa    (orcl "10.94.133.114" "9101" "atgqa"))
	  (atgprd   (orcl "10.183.238.84" "9101" "atgprd"))
	  (atgcaprd (orcl "10.183.238.84" "9101" "atgcaprd")))

      ;; Define our connections as an alist
      (let ((cs `(("devcore"      . ("core_dev"                "K0hl3r_4u"          ,atgdev))
		  ("devpcore"     . ("pcore_dev"               "K0hl3r_4u"          ,atgdev))
		  ("devcata"      . ("cataloga_dev"            "K0hl3r_4u"          ,atgdev))
		  ("devcatb"      . ("cataloga_dev"            "K0hl3r_4u"          ,atgdev))
		  ("devpcata"     . ("pcataloga_dev"           "K0hl3r_4u"          ,atgdev))
		  ("devpcatb"     . ("pcataloga_dev"           "K0hl3r_4u"          ,atgdev))
		  ("devpub"       . ("publishing_dev"          "K0hl3r_4u"          ,atgdev))

		  ("preprodcore"  . ("core_preprod"            "core_preprod"       ,atgqa))
		  ("preprodpcore" . ("pcore_preprod"           "pcore_preprod"      ,atgqa))
		  ("preprodcata"  . ("cataloga_preprod"        "cataloga_preprod"   ,atgqa))
		  ("preprodcatb"  . ("cataloga_preprod"        "cataloga_preprod"   ,atgqa))
		  ("preprodpcata" . ("pcataloga_preprod"       "pcataloga_preprod"  ,atgqa))
		  ("preprodpcatb" . ("pcataloga_preprod"       "pcataloga_preprod"  ,atgqa))
		  ("preproddval"  . ("catalog_data_validation" "cataloga"           ,atgqa))
		  ("preprodpub"   . ("publishing_preprod"      "publishing_preprod" ,atgqa))

		  ("qa2core"      . ("core2"                   "core2"              ,atgqa))
		  ("qa2pcore"     . ("pcore2"                  "pcore2"             ,atgqa))
		  ("qa2cata"      . ("cataloga_qa2"            "cataloga_qa2"       ,atgqa))
		  ("qa2catb"      . ("cataloga_qa2"            "cataloga_qa2"       ,atgqa))
		  ("qa2pcata"     . ("pcataloga_qa2"           "pcataloga_qa2"      ,atgqa))
		  ("qa2pcatb"     . ("pcataloga_qa2"           "pcataloga_qa2"      ,atgqa))
		  ("qa2pub"       . ("publishing_qa2"          "publishing_qa2"     ,atgqa))

		  ("prodcore"     . ("core2"                   "core2"              ,atgprd))
		  ("prodpcore"    . ("pcore2"                  "pcore2"             ,atgprd))
		  ("prodcata"     . ("cataloga"                "cataloga"           ,atgprd))
		  ("prodcatb"     . ("catalogb"                "catalogb"           ,atgprd))
		  ("prodpcata"    . ("pcataloga"               "pcataloga"          ,atgprd))
		  ("prodpcatb"    . ("pcatalogb"               "pcatalogb"          ,atgprd))
		  ("prodpub"      . ("publishing"              "publishing"         ,atgcaprd)))))

	;; Interactively display list of keys from alist, then look selection up in alist
	;; Finally, use destructuring-bind to set the internal default variables -
	;; because of this, user only needs to hit enter thrice to connect
	(destructuring-bind (cname sql-user sql-password sql-database)
	    (assoc (ido-completing-read "Open Kohler DB: " (mapcar 'car cs)) cs)

	  ;; This is the interactive *SQL* connection entry point
	  (message "Connecting to %s db endpoint" cname)
	  (sql-oracle))))))

;; Set a global key chord to something appropriate
(global-set-key (kbd "C-c KKK") 'my/kohler-db)
