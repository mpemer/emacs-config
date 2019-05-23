(progn
  (mapcar 'ensure-package-installed '(
				      bbdb
				      bbdb-ext
				      bbdb-csv-import
				      bbdb-vcard
				      ))
  (use-package bbdb)
  (use-package bbdb-ext)
  (use-package bbdb-csv-import)
  (use-package bbdb-vcard)
  )
