;;;; package --- Summary
;;
;; This file contains settings for usage of mu4e in emacs.
;;

;;; Commentary:


(require 'mu4e)

;;; Code:

;;(progn
;;  (ensure-package-installed 'mu4e)
;;  (use-package mu4e))

;; (progn
;;   (ensure-package-installed 'ivy)
;;   (use-package ivy))

;; (progn
;;   (ensure-package-installed 'mu4e-views)
;;   (use-package mu4e-views
;;     :after mu4e
;;     :defer nil
;;     :bind (:map mu4e-headers-mode-map
;; 		("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
;; 		("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
;; 		("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
;; 		("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
;; 		)
;;     :config
;;     (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
;;     (setq mu4e-views-default-view-method "html") ;; make xwidgets default
;;     (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
;;     (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
;;     (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view
;;   )

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
(setq user-full-name "Marcus Pemer")
(setq user-mail-address "marcus@iteego.com")
;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.

;; smtp mail setting; these are the same that `gnus' uses.
(setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "127.0.0.1"
   smtpmail-smtp-server         "127.0.0.1"
   smtpmail-smtp-service        1025
   smtpmail-local-domain        "pemer.io"

   mu4e-sent-folder             "/.Sent"
   mu4e-drafts-folder           "/.Drafts"
   mu4e-trash-folder            "/.Trash"

   mu4e-context-policy          'pick-first
   mu4e-compose-context-policy  'always-ask
   mu4e-compose-format-flowed   t

   mu4e-compose-in-new-frame    t
   mu4e-sent-messages-behavior  'delete

   mu4e-update-interval         300
   mu4e-headers-auto-update     t

   mu4e-view-show-addresses     t

   mu4e-confirm-quit            nil

;;   mu4e-view-prefer-html        nil
;;   mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
;;   shr-color-visible-luminance-min 80

   mu4e-attachment-dir  "~/Downloads"

   mu4e-maildir-shortcuts       '(("/INBOX"   . ?i)
				  ("/.Sent"   . ?s)
				  ("/.Trash"  . ?t)
				  ("/.Drafts" . ?d))

)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Alternatives are the following, however in first tests they
;; show inferior results
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
(setq mu4e-html2text-command "w3m -dump -T text/html")
;;(setq mu4e-html2text-command "pandoc -f html -t org")


(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
	   (flyspell-mode)))

(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "iteego"
    :enter-func (lambda () (mu4e-message "Entering context iteego"))
    :leave-func (lambda () (mu4e-message "Leaving context iteego"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "marcus@iteego.com")))
    :vars '((user-mail-address . "marcus@iteego.com")
	    (user-full-name . "Marcus Pemer")
	    (mu4e-compose-signature . (concat "--
With warm regards,

Marcus Pemer
President and CEO, Iteego
Mobile:   +1-310-880-7098
Email:    marcus@iteego.com
LinkedIn: http://www.linkedin.com/in/marcuspemer

PGP: 2DA555D654BCD05C

DISCLAIMER: This message contains information which may be confidential and/or privileged, and is intended only for the use of the addressee(s). Any use, disclosure or copying of this message or any part thereof by any person other than such addressee(s) is unauthorized and strictly prohibited. If you have received this message in error, please notify the sender by return e-mail and delete the message. Thank you for your cooperation."))
	    ))

   (make-mu4e-context
    :name "pemer.io"
    :enter-func (lambda () (mu4e-message "Entering context pemer.io"))
    :leave-func (lambda () (mu4e-message "Leaving context pemer.io"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "marcus@pemer.io")))
    :vars '((user-mail-address . "marcus@pemer.io")
	    (user-full-name . "Marcus Pemer")
	    (mu4e-compose-signature . (concat "--\nMarcus Pemer\nPGP: 2DA555D654BCD05C"))
	    ))

   (make-mu4e-context
    :name "pemer.com"
    :enter-func (lambda () (mu4e-message "Entering context pemer.com"))
    :leave-func (lambda () (mu4e-message "Leaving context pemer.com"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "marcus@pemer.com")))
    :vars '((user-mail-address . "marcus@pemer.com")
	    (user-full-name . "Marcus Pemer")
	    (mu4e-compose-signature . (concat "--\nMarcus Pemer\nPGP: 2DA555D654BCD05C"))
	    ))
   ))
