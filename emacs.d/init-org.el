
(require 'ical2org)

;; Calendar (M-x calendar)
(setq calendar-date-style 'european) ; format jour/mois/an
(setq calendar-week-start-day 1)     ; week starts on monday

(defvar calendar-day-name-array
  ["dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"])
(defvar calendar-day-abbrev-array
  ["dim" "lun" "mar" "mer" "jeu" "ven" "sam"])
(defvar calendar-month-name-array
  ["janvier" "février" "mars" "avril" "mai" "juin"
   "juillet" "août" "septembre" "octobre" "novembre" "décembre"])
(defvar calendar-month-abbrev-array
  ["jan" "fév" "mar" "avr" "mai" "jun" "jul" "aoû" "sep" "oct" "nov" "déc"])


(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg$" . org-mode))
(setq org-log-done t)
(setq org-return-follows-link t)
(setq org-completion-use-ido t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-hide-leading-stars t)

(setq org-agenda-files '("~/.org/work.org.gpg"
                         "~/.org/home.org.gpg"
                         "~/.org/agenda.org.gpg"))
;; (setq org-agenda-files '("~/.org/"))
;; (setq org-agenda-file-regexp "\\`[^.].*\\.org")
(setq org-directory "~/.org/")

(setq org-agenda-custom-commands
      '(("c" "Desk Work" todo "TODO"
         ((org-agenda-files '("~/.org/work.org.gpg"))
          (org-agenda-sorting-strategy '(priority-up effort-down)))
         ("~/work.html"))
        ))

(defun gtd ()
   (interactive)
   (find-file (concat org-directory "/home.org.gpg")))
(defun gtdwork ()
   (interactive)
   (find-file (concat org-directory "/work.org.gpg")))
(defun agenda ()
   (interactive)
   (find-file (concat org-directory "/agenda.org.gpg")))

(require 'remember)
(org-remember-insinuate)

;; Notes file
(setq org-default-notes-file (concat org-directory "/notes.org.gpg"))
;; Notes templates
(setq org-remember-templates
 '(("Note" ?n   "* NOTE %?\n %i\n %a" (concat org-directory "/notes.org.gpg") "Notes")
   ("Download" ?d "* DL %?\n %i\n %a" (concat org-directory "/notes.org.gpg") "Download")
   ("Todo" ?t   "* TODO %?\n %i\n %a" (concat org-directory "/notes.org.gpg") "Tasks")
   ("Idea" ?i "* %^{Title}\n %i\n %a" (concat org-directory "/notes.org.gpg") "Brainstorm")))

(defadvice remember-other-frame (around remember-frame-parameters activate)
  "Set remember frame params. call: emacsclient -e '(remember-other-frame)'"
  (let ((default-frame-alist (append '((name . "*Remember*")
                                       (width . 80)
                                       (height . 20)
                                       (vertical-scroll-bars . nil)
                                       (menu-bar-lines . 0)
                                       (tool-bar-lines . 0)
                                       )
                                     default-frame-alist)))
    ad-do-it))

(provide 'init-org)