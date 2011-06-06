(load "~/.emacs.d/site-lisp/ical2org/ical2org.el")

;; (require 'ical2org)

(defun s/ical2org ()
  "Convert ics files to org-mode"
  (interactive)
  (ical2org/convert-file "~/agenda/stef.ics" "~/agenda/stef.org")
)

