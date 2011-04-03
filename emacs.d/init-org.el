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
(require 'org-mouse)                 ; Extended mouse functionality
;; (load "~/.emacs.d/org/lisp/org-mouse.el")

(setq org-directory "~/.org/")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg$" . org-mode))

(setq org-log-done t)
(setq org-return-follows-link t)
(setq org-completion-use-ido t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-hide-leading-stars t)

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Pass mailto links to Alpine instead of browse-url
;; (setq org-link-mailto-program
;;  '(shell-command "urxvt -title Alpine -e alpine -url 'mailto:%a?Subject=%s'")
;; )

(setq org-agenda-files '("~/.org/work.org.gpg"
                         "~/.org/home.org.gpg"
                         "~/.org/agenda.org.gpg"))

(setq org-agenda-custom-commands
      '(("c" "Desk Work" todo "TODO"
         ((org-agenda-files '("~/.org/work.org.gpg"))
          (org-agenda-sorting-strategy '(priority-up effort-down)))
         ("~/work.html"))
        ))

(setq org-tag-alist '(("computer" . ?c)
                      ("home" . ?h)
                      ("project" . ?p)
                      ("mail" . ?m)
                      ("web" . ?w)
                      ("phone" . ?p)))

;{{{ Quick access to OrgMode and the OrgMode agenda
;    - org-mode configuration defined below
;
(defun s/org-index ()
   "Show the main org file."
   (interactive)
   (find-file (concat org-directory "/home.org.gpg")))
(defun s/org-work ()
   (interactive)
   (find-file (concat org-directory "/work.org.gpg")))
(defun s/org-agenda ()
  "Show the org-mode agenda."
  (interactive)
  (call-interactively 'org-agenda-list)
)

(defalias 'org           's/org-index)
(defalias 'agenda        's/org-agenda)

;}}}

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

;; Org-remember splits windows, force it to a single window
(add-hook 'remember-mode-hook  'delete-other-windows)

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

;; Automatic closing of remember frames
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
    (delete-frame))
)
(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
    (delete-frame))
)

;; Initialization of remember frames
(defun make-remember-frame ()
  "Create a new frame and run org-remember"
  (interactive)
  (make-frame '((name . "*Remember*") (width . 80) (height . 10)))
  (select-frame-by-name "*Remember*")
  (org-remember)
)


(require 'ical2org)


(provide 'init-org)