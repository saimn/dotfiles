;{{{ Calendar (M-x calendar)
;
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

;; calendar
(require 'calfw-org)
(require 'calfw-ical)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;; Another unicode chars
;; (setq cfw:fchar-junction ?╬
;;       cfw:fchar-vertical-line ?║
;;       cfw:fchar-horizontal-line ?═
;;       cfw:fchar-left-junction ?╠
;;       cfw:fchar-right-junction ?╣
;;       cfw:fchar-top-junction ?╦
;;       cfw:fchar-top-left-corner ?╔
;;       cfw:fchar-top-right-corner ?╗)

;}}}

;{{{ Org
;
(require 'org-install)
(require 'org-mouse)                 ; Extended mouse functionality
;; (load "~/.emacs.d/org/lisp/org-mouse.el")

(setq org-directory "~/org/")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg$" . org-mode))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-completion-use-ido t)
(setq org-hide-leading-stars t)               ; leading stars become invisible
(setq org-log-done t)                         ; add timestamp when done
(setq org-odd-levels-only t)                  ; skip all the even levels
(setq org-return-follows-link t)
(setq org-startup-indented t)                 ; turn on org-indent-mode

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Pass mailto links to Alpine instead of browse-url
;; (setq org-link-mailto-program
;;  '(shell-command "urxvt -title Alpine -e alpine -url 'mailto:%a?Subject=%s'")
;; )

(setq org-agenda-files '("~/org/work.org"
                         "~/org/home.org"
                         "~/org/agenda.org"
                         "~/org/stef.org"
                         ))

(setq org-agenda-custom-commands
      '(("c" "Desk Work" todo "TODO"
         ((org-agenda-files '("~/org/work.org"))
          (org-agenda-sorting-strategy '(priority-up effort-down)))
         ("~/work.html"))
        ))

(setq org-tag-alist '(("computer" . ?c)
                      ("home" . ?h)
                      ("project" . ?p)
                      ("mail" . ?m)
                      ("web" . ?w)
                      ("phone" . ?p)))

(setq org-agenda-custom-commands
      '(("h" "Agenda and tasks"
         ((agenda)
          (alltodo))
         nil
         ("~/agenda/agenda.html" "~/agenda/agenda.txt"))))

        ;; Examples :
        ;; ("X" agenda "" nil ("~/agenda/agenda.html" "~/agenda/agenda.txt"))
        ;; ("Y" alltodo "" nil ("~/agenda/todo.html" "~/agenda/todo.txt"))
        ;; ("h" "Agenda and Home-related tasks"
        ;;  ((agenda "")
        ;;   (tags-todo "home")
        ;;   (org-agenda-span month)
        ;;   ;; (tags "garden")
        ;;   )
        ;;  nil
        ;;  ("~/agenda/home.html"))
        ;; ("o" "Agenda and Office-related tasks"
        ;;  ((agenda)
        ;;   (tags-todo "work")
        ;;   (tags "office")
        ;;   (org-agenda-span month)
        ;;   )
        ;;  nil
        ;;  ("~/agenda/work.html" "~/agenda/work.ics"))

;; (add-hook 'org-finalize-agenda-hook 'org-store-agenda-views)

;}}}

;{{{ Quick access to OrgMode and the OrgMode agenda
;    - org-mode configuration defined below
;
(defun s/org-index ()
   "Show the main org file."
   (interactive)
   (find-file (concat org-directory "/")))
;; (defun s/org-work ()
;;    (interactive)
;;    (find-file (concat org-directory "/work.org")))
(defun s/org-agenda ()
  "Show the org-mode agenda."
  (interactive)
  (call-interactively 'org-agenda-list)
  (delete-other-windows)
)

(defalias 'org           's/org-index)
(defalias 'agenda        's/org-agenda)

;}}}

;{{{ Notifications
;
;; the appointment notification facility
(setq
  appt-message-warning-time 15 ;; warn 15 min in advance

  appt-display-mode-line t     ;; show in the modeline
  appt-display-format 'window) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

 ;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; our little façade-function for djcb-popup
(defun s/appt-display (min-to-app new-time msg)
  (s/popup (format "Appointment in %s minute(s)" min-to-app) msg
           "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
           "/usr/share/sounds/freedesktop/stereo/bell.oga"))
(setq appt-disp-window-function (function s/appt-display))
;}}}

;{{{ Remember
;
(require 'remember)
(org-remember-insinuate)

;; Notes file
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; Notes templates
(setq org-remember-templates
 '(("Note" ?n   "* NOTE %?\n %i\n %a" (concat org-directory "/notes.org") "Notes")
   ("Download" ?d "* DL %?\n %i\n %a" (concat org-directory "/notes.org") "Download")
   ("Todo" ?t   "* TODO %?\n %i\n %a" (concat org-directory "/notes.org") "Tasks")
   ("Idea" ?i "* %^{Title}\n %i\n %a" (concat org-directory "/notes.org") "Brainstorm")))

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
;}}}

(provide 'init-org)
