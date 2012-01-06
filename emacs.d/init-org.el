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

(require 'org-install)
(require 'org-mouse)                 ; Extended mouse functionality

(setq org-directory "~/org/")
(setq org-mobile-directory "/scpc:climbr@ssh.cluster003.ovh.net:mobileorg/")
(setq org-mobile-inbox-for-pull "~/org/mobileorg.org")
(setq org-archive-location "~/org/archives.org::")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg$" . org-mode))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-completion-use-ido t)
(setq org-hide-leading-stars t)               ; leading stars become invisible
(setq org-log-done t)                         ; add timestamp when done
;; (setq org-odd-levels-only t)               ; skip all the even levels
(setq org-return-follows-link t)
(setq org-startup-indented t)                 ; turn on org-indent-mode
;; (setq org-CUA-compatible t)		      ; disable keybindings for Shift+arrow keys

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Pass mailto links to Alpine instead of browse-url
;; (setq org-link-mailto-program
;;  '(shell-command "urxvt -title Alpine -e alpine -url 'mailto:%a?Subject=%s'")
;; )

;; (setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-agenda-files '("~/org/home.org" "~/org/work.org"))

(setq org-tag-alist '(("home" . ?h)
                      ("work" . ?w)
                      ("project" . ?p)
                      ("mail" . ?m)
                      ("computer" . ?c)
                      ("web" . ?o)
                      ("phone" . ?t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "PROJECT(p)" "WAITING(w)" "|")
        (sequence "REPORT(r)" "BUG(b)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)" "DEFERRED(e)")))

(setq org-agenda-custom-commands
      '(
        ("a" "Agenda for current week or day and tasks"
         ((agenda)
          (alltodo))
         nil
         ("~/org/views/agenda.html" "~/org/views/agenda.txt"))
        ("h" "Agenda and Home-related tasks"
         ((agenda))
         ((org-agenda-files '("~/org/home.org"))
          (org-agenda-compact-blocks t))
         ("~/org/views/home.html"))
        ("o" "Agenda and Office-related tasks"
         ((agenda))
         ((org-agenda-files '("~/org/work.org"))
          ;; (tags-todo "work")
          ;; (tags "office")
          )
         ("~/org/views/work.html" "~/org/views/work.ics"))
        ("W" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ; review upcoming deadlines and appointments
                                             ; type "l" in the agenda to review logged items
          ;; (stuck "")         ; review stuck projects as designated by org-stuck-projects
          (todo "PROJECT")   ; review all projects (assuming you use todo keywords to designate projects)
          (todo "MAYBE")     ; review someday/maybe items
          (todo "WAITING"))  ; review waiting items
         nil
         ("~/org/views/review.html"))
        ))

;; (add-hook 'org-finalize-agenda-hook 'org-store-agenda-views)
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

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

;{{{ Compile Org

(defvar my/org-lisp-directory "~/.emacs.d/org/lisp"
  "Directory where your org-mode files live.")

(defvar my/org-compile-sources t
  "If `nil', never compile org-sources. `my/compile-org' will only create
the autoloads file `org-install.el' then. If `t', compile the sources, too.")

;; Customize:
(setq my/org-lisp-directory "~/.emacs.d/site-lisp/org-mode/lisp")

;; Customize:
(setq  my/org-compile-sources t)

(defun my/compile-org(&optional directory)
  "Compile all *.el files that come with org-mode."
  (interactive)
  (setq directory (concat
                        (file-truename
                    (or directory my/org-lisp-directory)) "/"))

  (add-to-list 'load-path directory)

  (let ((list-of-org-files (file-expand-wildcards (concat directory "*.el"))))

    ;; create the org-install file
    (require 'autoload)
    (setq esf/org-install-file (concat directory "org-install.el"))
    (find-file esf/org-install-file)
    (erase-buffer)
    (mapc (lambda (x)
            (generate-file-autoloads x))
          list-of-org-files)
    (insert "\n(provide (quote org-install))\n")
    (save-buffer)
    (kill-buffer)
    (byte-compile-file esf/org-install-file t)

    (dolist (f list-of-org-files)
      (if (file-exists-p (concat f "c")) ; delete compiled files
          (delete-file (concat f "c")))
      (if my/org-compile-sources     ; Compile, if `my/org-compile-sources' is t
          (byte-compile-file f)))))
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
      '(("Note" ?n   "* NOTE %?\n %i\n %a" "notes.org" "Notes")
        ("Download" ?d "* DL %?\n %i\n %a" "notes.org" "Download")
        ("Todo" ?t   "* TODO %?\n %i\n %a" "home.org" "TASKS")))

;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/org/home.org" "TASKS") "* TODO %?\n %i\n %a")
;;         ("n" "Notes" entry (file+datetree "~/org/notes.org") "* %?\nEntered on %U\n %i\n %a")))

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
