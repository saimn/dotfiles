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

;; disable vc to speed up saves
;(setq vc-handled-backends nil)

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

;; use the Trash for file deletion
;(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-aFGhl --group-directories-first")

;;----------------------------------------------------------------------
;; Interface
;;----------------------------------------------------------------------
(require 'init-ui)
(require 'init-edit)
(require 'init-completion)

;; Installer avant color-theme
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-wombat)
(require 'color-theme-tangotango)
(setq frame-background-mode 'dark)

(color-theme-tangotango)

;; (if window-system
;;      (color-theme-tangotango)
;;    (some-term-theme))

;; (require 'color-theme-zenburn)
;; (color-theme-zenburn)

;;----------------------------------------------------------------------
;; Browser
;;----------------------------------------------------------------------
;; Default Web Browser
(setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "/usr/bin/firefox")

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

;; Tramp (remote files editing)
;; (require 'tramp)
(setq tramp-default-method "ssh")

;; Folding
;;   - http://www.emacswiki.org/emacs/FoldIngo
;; (require 'foldingo)

;; distinguish files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;----------------------------------------------------------------------
;; Version Control
;;----------------------------------------------------------------------
(autoload 'magit-status "magit" nil t)
(autoload 'svn-status "psvn" nil t)
(setq vc-svn-diff-switches 'nil)
(setq vc-diff-switches '("-bBu"))

;;----------------------------------------------------------------------
;; Pymacs and Rope for Python
;;----------------------------------------------------------------------
(when (require 'pymacs nil 'noerror)
  ;; (eval-after-load "pymacs"
  ;;  '(add-to-list 'pymacs-load-path "~/.emacs.d/site-lisp/"))
  (setq pymacs-load-path (list (expand-file-name "~/.emacs.d/site-lisp")))

  ;; some python tools
  ;; (pymacs-load "python")
  ;; python refactoring and support
  ;;(pymacs-load "ropemacs" "rope-")
  ;; shortcut function to insert license headers
  (pymacs-load "license")

  ;; rope settings
  (setq ropemacs-guess-project t
        ropemacs-enable-autoimport t))

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
(global-set-key (kbd "C-c r") 'org-remember)

(global-set-key (kbd "C-c c") 'server-edit)
(global-set-key (kbd "C-c f") 'flyspell-buffer)
;; (global-set-key (kbd "C-c h") 'htmlfontify-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)
(global-set-key (kbd "C-c q") 'refill-mode)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c t") 'trim-whitespace)
(global-set-key (kbd "C-c v") 'refill-mode)
(global-set-key (kbd "C-c x") 'shell)
(global-set-key (kbd "C-c y") '(lambda ()
                                 (interactive)
                                 (popup-menu 'yank-menu)))

;; fonction keys
(global-set-key [f1]  (lambda () (interactive) (manual-entry (current-word))))
(global-set-key [f2]  (lambda () (interactive) (find-file "~/org/notes.org")))
(global-set-key [f3]  's/org-agenda)
(global-set-key [f5]  'kill-this-buffer)
(global-set-key [f6]  'repeat-complex-command)
(global-set-key [f7]  'other-window)
(global-set-key [f8]  'deft)
(global-set-key [f9]  'compile)
(global-set-key [f10] 'my-toggle-menu-and-scrollbar)
(global-set-key [f11] 'toggle-truncate-lines)
(global-set-key [f12] 'grep)

;; (global-set-key (kbd "<f8>") 'query-replace)
;; (global-set-key (kbd "S-<f8>") 'query-replace-regexp)

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
  (set compose-mail-check-user-agent nil)
  (flyspell-mode)
  ;; (abbrev-mode 1)
  (setq sc-nested-citation-p t)
  (local-set-key "\C-Xk" 'server-edit))
(add-hook 'mail-mode-hook 'my-mail-mode-hook)
(add-hook 'mail-citation-hook 'sc-cite-original)

(autoload 'sc-cite-original "supercite" nil t)
(autoload 'muttrc-mode "muttrc-mode.el"
  "Major mode to edit muttrc files" t)
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode))
              auto-mode-alist))

;;----------------------------------------------------------------------
;; Config
;;----------------------------------------------------------------------
(require 'init-text)      ; Markdown, rst, bbcode, ...
(require 'init-spell)     ; spell check
(require 'init-functions) ; Useful functions
(require 'init-org)       ; Org-mode
(require 'init-latex)     ; LaTeX mode
(require 'init-c)         ; C
(require 'init-python)    ; Python
;; (require 'init-haskell)   ; Haskell
(require 'init-idlwave)   ; IDL - IDLwave
(require 'init-lisp)      ; Emacs-Lisp
(require 'init-web)       ; PHP - HTML - CSS
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

;;  Perl
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
