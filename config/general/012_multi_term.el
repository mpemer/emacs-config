(progn
  (ensure-package-installed 'multi-term)
  (use-package multi-term))
;;(setf multi-term-program "/home/mpemer/.nvm/versions/node/v9.11.2/bin/closh")
;;     :config (progn
;; 	      (setf multi-term-program "sh -c /home/mpemer/bin/closh-zero.jar")
;; 	      (defun multi-term-tmux ()
;;                 "Make a multi-term buffer running tmux."
;;                   (let ((multi-term-program "tmux"))
;;                     (multi-term)))
;; 	      (defun multi-term-kohler-stg-app ()
;;                 "Make a multi-term buffer running ssh to kohler staging app."
;;                   (let ((multi-term-program "sh -c 'ssh -t kohler-ibm-stg-app closh-zero.jar'"))
;;                     (multi-term)))
;; ;;	      (defun term-send-esc ()
;; ;;		"Send ESC in term mode."
;; ;;		(interactive)
;; ;;		(term-send-raw-string "\e"))
;; 	      )))
	      
;; ;;	       (setq multi-term-program "tmux"))))

;; ;;	       (setq multi-term-program "java -jar ~/bin/closh-zero.jar"))))

;; (defun multi-term-tmux ()
;;   "Make a multi-term buffer running tmux."
;;   (let ((multi-term-program "tmux"))
;;     (multi-term)))
;; (defun multi-term-kohler-stg-app ()
;;   "Make a multi-term buffer running ssh to kohler staging app."
;;   (let ((multi-term-program "sh -c 'ssh -t kohler-ibm-stg-app closh-zero.jar'"))
;;     (multi-term)))
