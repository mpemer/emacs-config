;;; -*- mode: Lisp; epa-file-encrypt-to: ("1D151FF890EE620251BC79A4E594D6C2CC9E1BAA") -*-
;;; Package --- Summary:

;;; Commentary:

;;; Code:


;; ORG-MODE CONFIG CHANGES

;;; Code:
(require 'defs)

(setq org-crypt-key "1D151FF890EE620251BC79A4E594D6C2CC9E1BAA")

(setq org-caldav-oauth2-client-id
      "674574086916-9krsuhppcjvldap7ukrn0o1ua63eumg3.apps.googleusercontent.com"

      org-caldav-oauth2-client-secret
      "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"

      org-caldav-url 'google

      plstore-cache-passphrase-for-symmetric-encryption t)



(setq org-caldav-calendars
      '((:calendar-id "pemer.com_d6a79it0p9hrimh3mnvva1r3pg@group.calendar.google.com"
         :files ("~/org/plan.org")
         :inbox "~/org/plan.org")
        (:calendar-id "pemer.com_oa4sm4dsh7hh24r4ceghjqog44@group.calendar.google.com"
         :files ("~/org/family.org")
         :inbox "~/org/family.org")
        (:calendar-id "marcus@pemer.com"
         :files ("~/org/personal.org")
         :inbox "~/org/personal.org")
        (:calendar-id "marcus@iteego.com"
         :files ("~/org/iteego.org")
         :inbox "~/org/iteego.org")
        ))


(provide 'mpemer_011_org)
;;; mpemer_011_org.el ends here
