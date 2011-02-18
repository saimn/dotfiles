;;-*- Mode: Emacs-Lisp -*-
;; .emacs - Emacs configuration file

(message "Loading ~/.emacs.d/init.el")

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

;; Turn off mouse interface early in startup to avoid momentary display
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(server-start)                        ; enable emacsclient
(setq major-mode 'text-mode)          ; start in text-mode
(setq inhibit-startup-message t)      ; Don't show startup buffer
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Traite 'y' ou <CR> comme yes, 'n' comme no.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Mouse
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)
;; (setq scroll-step 1)

;;----------------------------------------------------------------------------
;; paths
;;----------------------------------------------------------------------------

;; Load Path
;; (add-to-list 'load-path "~/.emacs.d")
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; backup files
;; (setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup-files/")))

;; Customize file
(setq custom-file (expand-file-name "~/.emacs.d/customize.el"))
(load custom-file)

;(setq delete-by-moving-to-trash t)

;;----------------------------------------------------------------------
;; Interface
;;----------------------------------------------------------------------
(require 'init-ui)
(require 'init-completion)

;; Installer avant color-theme
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-wombat)
(require 'color-theme-tangotango)
(color-theme-tangotango)
(setq frame-background-mode 'dark)

;; (require 'zenburn)
;; (unless (zenburn-format-spec-works-p)
;;   (zenburn-define-format-spec))

;;----------------------------------------------------------------------
;; Browser
;;----------------------------------------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox")

;; (defun uzbl-browse (url &rest ignore)
;;   "Browse URL using uzbl."
;;   (interactive "sURL: ")
;;   (shell-command (concat "uzbl-browser " url))
;;   (pop-to-buffer "*Shell Command Output*")
;;   (setq truncate-lines t))

;; (defun choose-browser (url &rest args)
;;   (interactive "sURL: ")
;;   (if (y-or-n-p "Use external browser? ")
;;       (browse-url-generic url)
;;     (uzbl-browse url)))

;; (setq browse-url-browser-function 'choose-browser)
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
                               (mode . lua-mode)
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

;; Ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; EasyPG - enabled by default
;; (require 'epa-file)
;; (epa-file-enable)

;; distinguish files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'twittering-mode)
(eval-after-load "twittering-mode"
  '(progn
     (twittering-icon-mode)))            ; Show icons
(setq twittering-timer-interval 900      ; Update your timeline each x sec
      ;; twittering-tinyurl-service 'bit.ly
      twittering-use-master-password t)

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
;; Version Control
;;----------------------------------------------------------------------

(autoload 'magit-status "magit" nil t)
(autoload 'svn-status "psvn" nil t)
(setq vc-svn-diff-switches 'nil)
(setq vc-diff-switches '("-bBu"))

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
(setq next-line-add-newlines t)   ; C-n add new line

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
;; - space      => montrer les espaces
;; - tabs       => montrer les tabulations
;; - trailing   => montrer les espaces superflus en fin de ligne
;; - lines-tail => montrer les lignes trop longues (excédent seulement)
;;
;; « M-x whitespace-cleanup »: déclenche un nettoyage du tampon
;; Cette commande lancera les actions annoncées dans whitespace-style :
;; - empty            => suppression des lignes vides en début et fin de tampon
;; - indentation      => remplace les tabulations de début de ligne par des espaces
;; - space-before-tab => remplace les tabulations précédant des espaces par des espaces
;; - space-after-tab  => remplace les tabulations suivant des espaces par des espaces
;; - trailing         => suppression des espaces et tabulations superflus en fin de ligne

;; make whitespace-mode use just basic coloring
(setq whitespace-style '(tabs trailing lines-tail newline))
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
;; (global-set-key (kbd "C-c h") 'htmlfontify-buffer)
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
;; Config
;;----------------------------------------------------------------------
(require 'init-text)      ; Markdown, rst, bbcode, ...
;(require 'init-spell)     ; spell check
(require 'init-functions) ; Useful functions
(require 'init-org)       ; Org-mode
(require 'init-latex)     ; LaTeX mode
(require 'init-c)         ; C
(require 'init-python)    ; Python
;; (require 'init-haskell)   ; Haskell
(require 'init-idlwave)   ; IDL - IDLwave
(require 'init-lisp)      ; Emacs-Lisp
(require 'init-html)      ; PHP - HTML - CSS
(require 'init-tags)      ; Ctags

;;----------------------------------------------------------------------
;; Other prog modes
;;----------------------------------------------------------------------

;; shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up] 'comint-previous-input)
     (define-key shell-mode-map [down] 'comint-next-input)
     (define-key shell-mode-map "\C-p" 'comint-previous-input)
     (define-key shell-mode-map "\C-n" 'comint-next-input)))

;; pkgbuild
(autoload 'pkgbuild-mode "pkgbuild-mode" nil t)

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

;; Lua
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

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
      ("\\.conf\\'" . conf-mode)
      ("\\rc\\'" . conf-mode)
      ("/mutt" . mail-mode)
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
