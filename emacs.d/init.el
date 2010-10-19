;;-*- Mode: Emacs-Lisp -*-
;; .emacs - Emacs configuration file
;; Time-stamp: <2010-10-19 12:02>

;; (message "Loading ~/.emacs/init.el")

;;----------------------------------------------------------------------
;; Help
;;----------------------------------------------------------------------

;; # Useful links & inspiration
;; Sébastien Dinot - http://sebastien.dinot.free.fr/emacs.html
;; Philippe Dumont - http://www.lifl.fr/~dumont/emacs/
;; http://www-verimag.imag.fr/~moy/emacs/
;; http://www.hollenback.net/emacs/emacs.el
;; http://people.via.ecp.fr/~flo/2000/emacs-tut/
;; EmacsWiki (http://www.emacswiki.org/)
;; Emacs-Fu (http://emacs-fu.blogspot.com/)
;; Xah Lee (http://xahlee.org/emacs/)

;;----------------------------------------------------------------------
;; Global settings
;;----------------------------------------------------------------------

(setq user-full-name "Simon")
(setq user-mail-address "sc at sconseil.fr")

;; enable emacsclient
(server-start)

;; start in text-mode
(setq major-mode 'text-mode)

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Traite 'y' ou <CR> comme yes, 'n' comme no.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Don't show startup buffer
(setq inhibit-startup-message t)

;; Mouse
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)
;; (setq scroll-step 1)

;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; Load Path
;; (add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/themes")

;; disable backup files
;; (setq make-backup-files nil)
;; backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup-files/")))

;; Customize file
(setq custom-file (expand-file-name "~/.emacs.d/customize.el"))
(load custom-file)

;;----------------------------------------------------------------------
;; Interface
;;----------------------------------------------------------------------
(require 'init-ui)

;; Installer avant color-theme
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-wombat)
(require 'color-theme-tangotango)
(require 'zenburn)
;; (color-theme-vim-colors)
(color-theme-tangotango)
(setq frame-background-mode 'dark)

(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))

;;----------------------------------------------------------------------
;; Browser
;;----------------------------------------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox")

(defun uzbl-browse (url &rest ignore)
  "Browse URL using uzbl."
  (interactive "sURL: ")
  (shell-command (concat "uzbl-browser " url))
  (pop-to-buffer "*Shell Command Output*")
  (setq truncate-lines t))

(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-generic url)
    (uzbl-browse url)))

(setq browse-url-browser-function 'choose-browser)
(global-set-key "\C-xm" 'browse-url-at-point)

;;----------------------------------------------------------------------
;; Buffers
;;----------------------------------------------------------------------

;; Show all buffer names when you switch buffers with C-x b
;; (iswitchb-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Config" (filename . ".emacs.d/"))
               ("Dired" (mode . dired-mode))
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Buffer List\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*vc\\*$")
                         (name . "^\\*Warnings\\*$")))
               ("IDL" (or
                       (mode . idlwave-mode)
                       (name . "^\\*idl\\*$")))
               ("Org" (or
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (mode . org-mode)
                       (mode . muse-mode)))
               ("Programming" (or
                               (mode . python-mode)
                               (mode . haskell-mode)
                               (name . "\\*Python.*\\*")
                               (name . "\\*haskell.*\\*")
                               (mode . emacs-lisp-mode)
                               (mode . sh-mode)))
               ("Writing" (or
                           (mode . tex-mode)
                           (mode . latex-mode)
                           (mode . markdown-mode)
                           (mode . rst-mode)))
               ;; ("gnus" (or
               ;;          (mode . message-mode)
               ;;          (mode . bbdb-mode)
               ;;          (mode . mail-mode)
               ;;          (mode . gnus-group-mode)
               ;;          (mode . gnus-summary-mode)
               ;;          (mode . gnus-article-mode)
               ;;          (name . "^\\.bbdb$")
               ;;          (name . "^\\.newsrc-dribble")))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;----------------------------------------------------------------------
;; Load modules / modes
;;----------------------------------------------------------------------

;; Show images et compressed files
(setq auto-image-file-mode t)
(setq auto-compression-mode t)

;; EasyPG
(require 'epa-file)
(epa-file-enable)

;; distinguish files with the same name
;; (require 'uniquify)

(require 'magit)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;----------------------------------------------------------------------
;; twitter
;;----------------------------------------------------------------------
(require 'twittering-mode)
(setq twittering-icon-mode t)         ; Show icons
(setq twittering-timer-interval 900)  ; Update your timeline each x sec
(setq twittering-use-master-password t)

(add-hook 'twittering-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(("F" . twittering-friends-timeline)
                    ("R" . twittering-replies-timeline)
                    ("U" . twittering-user-timeline)
                    ("W" . twittering-update-status-interactive)))))

(add-hook 'twittering-new-tweets-hook (lambda ()
  (let ((n twittering-new-tweets-count))
    (start-process "twittering-notify" nil "notify-send"
                   "-i" "/usr/share/pixmaps/gnome-emacs.png"
                   "New tweets"
                   (format "You have %d new tweet%s"
                           n (if (> n 1) "s" ""))))))

;;----------------------------------------------------------------------
;; Completion
;;----------------------------------------------------------------------

;(setq tab-always-indent 'complete)
; Completion of acronyms and initialisms
;(setq completion-styles (append completion-style '(initials)))

;; minibuffer completion
(icomplete-mode) ; complete as you type
(setq completion-ignored-extensions '(".o" "~" ".bin" ".aux"))
(partial-completion-mode)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets")

;; InteractivelyDoThings
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; enable fuzzy matching
(setq ido-everywhere t)           ; use ido everywhere
(setq ido-show-dot-for-dired t)   ; always show dot for current directory

(require 'smart-tab)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'idlwave-mode)
(global-auto-complete-mode t)

;; C-n/C-p to select candidates
;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; enable autopair in all buffers
(require 'autopair)
(autopair-global-mode)

;;----------------------------------------------------------------------
;; Edit
;;----------------------------------------------------------------------

(setq-default fill-column 78)     ; auto-fill
(setq comment-auto-fill-only-comments t)
(global-visual-line-mode 1)       ; Wrap Long Lines By Word Boundary
;; (setq longlines-wrap-follows-window-size t)
(show-paren-mode 1)               ; turn on paren match highlighting
(transient-mark-mode t)           ; highlight selection (default on Emacs 23)
(delete-selection-mode 1)         ; delete seleted text when typing
(global-font-lock-mode t)         ; coloration syntaxique
(setq font-lock-maximum-size nil) ; Mettre un maximum de couleurs
(hl-line-mode 1)                  ; highlight current line
(auto-revert-mode 1)              ; detect changes on file
;; (global-linum-mode 1)           ; display line numbers in margin (Emacs 23)
(setq kill-whole-line t)          ; kill-line including the line ending char
(setq blink-cursor-mode nil)

(put 'downcase-region 'disabled nil) ; enable region-down(up)casing
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)  ; Indent with spaces
(setq c-basic-offset 2)              ; default indent: 2 spaces

;; No truncate line -> replaced by visual-line-mode
;; (setq truncate-lines nil)
;; (setq truncate-partial-width-windows nil)

;; find special words
(add-hook 'find-file-hooks
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\<\\(FIXME\\|TODO\\|WARNING\\|BUG\\|XXX+\\|NB\\|NOTE\\|ATTENTION\\)[  ]*:"
         1 font-lock-warning-face prepend))
    )
  )
)

;;----------------------------------------------------------------------
;; Save
;;----------------------------------------------------------------------

;; demander s'il faut ajouter un saut de ligne final lorsqu'il est absent
;; (setq require-final-newline 'query)

;; delete end-of-line spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; actualiser l'année dans la mention de copyright
(add-hook 'write-file-functions 'copyright-update)

;; Time-stamp - insert modification date with 'Time-stamp: <>'
(setq
  time-stamp-active 1
  time-stamp-line-limit 10
  ;; time-stamp-pattern "\\([Ll]ast modified\\|[Dd]erni[eè]re modification\\) *: %04y-%02m-%02d %02H:%02M$"
  time-stamp-format "%04y-%02m-%02d %02H:%02M")
(add-hook 'before-save-hook 'time-stamp)

;;----------------------------------------------------------------------
;; Whitespace mode
;;----------------------------------------------------------------------

;; Choix des éléments mis en exergue :
;; - space      => montrer les espaces (KO car gestion intégrée d'Emacs)
;; - tabs       => montrer les tabulations
;; - trailing   => montrer les espaces superflus en fin de ligne
;; - lines-tail => montrer les lignes trop longues (excédent seulement)
;;
;; On peut en outre déclencher un nettoyage du tampon via la commande
;; « M-x whitespace-cleanup ». Cette commande lancera les actions annoncées
;; dans whitespace-style :
;; - empty            => suppression des lignes vides en début et fin de
;;                        tampon
;; - indentation      => substitution des tabulations de début de ligne par
;;                        des espaces
;; - space-before-tab => substitution des tabulations précédant des espaces
;;                        par des espaces
;; - space-after-tab  => substitution des tabulations suivant des espaces par
;;                        des espaces
;; - trailing         => suppression des espaces et tabulations superflus en
;;                        fin de ligne
;;
;; Le mode whitespace interfère avec la visualisation des espaces insécables
;; intégrée à Emacs (variable nobreak-char-display) et je n'ai pas réussi à
;; imposer à Emacs le paramétrage que j'avais défini au sein du mode
;; whitespace (si une âme charitable a la solution, j'en suis preneur). J'ai
;; donc fini par altérer le paramètre « nobreak-space » qui, dans le mode
;; intégré à Emacs, détermine l'affichage des espaces insécables. Pour ce
;; faire, j'ai eu recours à « custom-set-faces » et pour éviter un double
;; appel de cette fonction (fortement déconseillé), j'ai déporté l'instruction
;; dans le fichier ~/.emacs.d/custom.el. Le voici pour mémoire :
;;
;; (custom-set-faces
;;   '(nobreak-space
;;     (;; Paramétrage si fond foncé et grand nombre de couleurs disponible
;;      (((class color) (min-colors 88) (background dark))
;;       (:background "#483838"       :foreground "black" :underline nil))
;;      ;; Paramétrage si fond clair et grand nombre de couleurs disponible
;;      (((class color) (min-colors 88) (background light))
;;       (:background "LemonChiffon3" :foreground "cornsilk4" :underline nil))
;;      ;; Paramétrage dans les autres cas de figure
;;      (t (:inverse-video t)))))

;; make whitespace-mode use just basic coloring
(setq whitespace-style '(tabs trailing lines-tail newline ))
;; empty indentation space-before-tab space-after-tab spaces

;; Nombre de colonnes au delà duquel on considère qu'une ligne est trop longue
(setq whitespace-line-column 78)

;; en Americain, les phrases (sentences) se terminent par deux espaces
;; ce comportement n'est pas souhaitable en francais
(setq sentence-end-double-space nil)

;; ;; face for long lines' tails
;; (set-face-attribute 'whitespace-line nil
;;                     :background "red1"
;;                     :foreground "yellow"
;;                     :weight 'bold)

;; ;; face for Tabs
;; (set-face-attribute 'whitespace-tab nil
;;                     :background "red1"
;;                     :foreground "yellow"
;;                     :weight 'bold)

;; Activation globale du mode mettant en exergue des « caractères blancs »
(global-whitespace-mode 1)

;; activate minor whitespace mode when in python mode
;(add-hook 'python-mode-hook 'whitespace-mode)

;;----------------------------------------------------------------------
;; Viper
;;----------------------------------------------------------------------

;; (setq viper-mode t)
;; (require 'viper)

;; (defadvice viper-maybe-checkout (around viper-svn-checkin-fix activate)
;;   "Advise viper-maybe-checkout to ignore svn files."
;;   (let ((file (expand-file-name (buffer-file-name buf))))
;;     (when (and (featurep 'vc-hooks)
;;                (not (memq (vc-backend file) '(nil SVN))))
;;       ad-do-it)))

;;----------------------------------------------------------------------
;; Keyboard shortcuts
;;----------------------------------------------------------------------

;; use cua-mode only for rectangles
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Mouse
(global-set-key [mouse-3] 'imenu)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "<C-escape>") 'ibuffer-list-buffers)
(global-set-key (kbd "C-/") 'comment-dwim-line)
(global-set-key "\M-;" 'comment-dwim-line)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-S-iso-lefttab>") '(lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-g") 'goto-line)

;; supprime le formatage du paragraphe courant
(global-set-key (kbd "M-Q") 'remove-hard-wrap-paragraph)

(global-set-key [kp-enter] 'newline-and-indent)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; (global-set-key [delete] 'delete-backward-char)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c r") 'remember)

(global-set-key (kbd "C-c c") 'server-edit)
(global-set-key (kbd "C-c f") 'flyspell-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c t") 'trim-whitespace)
(global-set-key (kbd "C-c v") 'refill-mode)
(global-set-key (kbd "C-c x") 'shell)
(global-set-key (kbd "C-c y") '(lambda ()
                                 (interactive)
                                 (popup-menu 'yank-menu)))

(global-set-key (kbd "<f5>") 'kill-this-buffer)
(global-set-key (kbd "<f6>") 'buffer-menu)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "<f8>") 'query-replace)
(global-set-key (kbd "S-<f8>") 'query-replace-regexp)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f10>") 'my-toggle-menu-and-scrollbar)
;; (global-set-key [f10] 'repeat-complex-command)
(global-set-key (kbd "<f11>") 'toggle-truncate-lines)
(global-set-key (kbd "<f12>") 'grep)

(global-set-key (kbd "<C-f1>") 'compile)
(global-set-key (kbd "<C-f3>") 'next-error)
(global-set-key (kbd "<C-f4>") 'previous-error)
(global-set-key (kbd "<C-f9>") 'recompile)
(global-set-key (kbd "<C-f10>") 'kill-compilation)

;;----------------------------------------------------------------------
;; Text mode
;;----------------------------------------------------------------------

;; Markdown, rst, bbcode, ...
(require 'init-text)

;; spell check
(require 'init-spell)

;;----------------------------------------------------------------------
;; Mail
;;----------------------------------------------------------------------

(defun my-mail-mode-hook ()
  (auto-fill-mode 1)
  (setq fill-column 72)
  ;(abbrev-mode 1)
  (local-set-key "\C-Xk" 'server-edit)
  )
(add-hook 'mail-mode-hook 'my-mail-mode-hook)

;;----------------------------------------------------------------------
;; Useful functions
;;----------------------------------------------------------------------
(require 'init-functions)

;;----------------------------------------------------------------------
;; Org-mode
;;----------------------------------------------------------------------
(require 'init-org)

;;----------------------------------------------------------------------
;; LaTeX mode
;;----------------------------------------------------------------------
(require 'init-latex)

;;----------------------------------------------------------------------
;; C
;;----------------------------------------------------------------------
(require 'init-c)

;;----------------------------------------------------------------------
;; Python
;;----------------------------------------------------------------------
(require 'init-python)

;;----------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------
(require 'init-haskell)

;;----------------------------------------------------------------------
;; IDL - IDLwave
;;----------------------------------------------------------------------
(require 'init-idlwave)

;;----------------------------------------------------------------------
;; mode Emacs-Lisp...
;;----------------------------------------------------------------------
(require 'init-lisp)

;;----------------------------------------------------------------------
;; PHP - HTML - CSS
;;----------------------------------------------------------------------
(require 'init-html)

;;----------------------------------------------------------------------
;; Ctags
;;----------------------------------------------------------------------
(require 'init-tags)

;;----------------------------------------------------------------------
;; Modes divers
;;----------------------------------------------------------------------

;; shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up] 'comint-previous-input)
     (define-key shell-mode-map [down] 'comint-next-input)
     (define-key shell-mode-map "\C-p" 'comint-previous-input)
     (define-key shell-mode-map "\C-n" 'comint-next-input)))

(require 'pkgbuild-mode)

;; mode Perl
;; (defalias 'perl-mode 'cperl-mode)
;; (defun my-perl-mode-hook ()
;;   (setq tab-width 4)
;;   (setq cperl-indent-level 2)
;;   (setq cperl-continued-statement-offset 0)
;;   (setq cperl-extra-newline-before-brace t)
;;   )
;; (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
;; (setq perl-indent-level 4)
;; (setq perl-continued-statement-offset 4)
;; (add-hook 'perl-mode-hook 'my-perl-mode-hook)

;; Fortran
;; (defun my-fortran-mode-hook ()
;;   (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
;; (add-hook 'f90-mode-hook 'my-fortran-mode-hook)

;;----------------------------------------------------------------------
;; Mode for each file type
;;----------------------------------------------------------------------
(setq auto-mode-alist
  (append
    '(("\\.[kz]?sh\\'" . sh-mode)
      ("bash" . sh-mode)
      ("profile" . sh-mode)
      ("[Mm]akefile\\'" . makefile-mode)
      ("\\.mk\\'" . makefile-mode)
      ("\\.p[lm]\\'" . cperl-mode)
      ("/mutt" . mail-mode)
      ("\\.conf\\'" . conf-mode)
      ("\\rc\\'" . conf-mode)
      ("\\PKGBUILD\\'" . pkgbuild-mode)
     )
     auto-mode-alist
  )
)

;;----------------------------------------------------------------------------
;; Private configuration
;;----------------------------------------------------------------------------
(if (file-readable-p
     (expand-file-name "~/.emacs.d/init-priv.el"))
    (require 'init-priv))
