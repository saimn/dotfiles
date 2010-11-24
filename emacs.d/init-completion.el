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

;; InteractivelyDoThings
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; enable fuzzy matching
(setq ido-everywhere t)           ; use ido everywhere
(setq ido-show-dot-for-dired t)   ; always show dot for current directory
;(setq ido-file-extensions-order '(".org" ".txt" ".py" ".el" ".ini" ".cfg"))

(require 'anything-config)
(defun my-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-emacs-commands
     anything-c-source-buffers
     anything-c-source-files-in-current-dir
     anything-c-source-file-name-history
     ;; anything-c-source-info-pages
     ;; anything-c-source-man-pages
     ;; anything-c-source-file-cache
     anything-c-source-locate)
   " *my-anything*"))
(global-set-key (kbd "M-X") 'my-anything)

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
(setq autopair-autowrap t)


;;----------------------------------------------------------------------
;; Templates
;;----------------------------------------------------------------------

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets")

(require 'autoinsert)
(auto-insert-mode)                                   ; Adds hook to find-files-hook
(setq auto-insert-directory "~/dotfiles/templates/") ; *NOTE* Trailing slash important
(setq auto-insert-query nil)                         ; don't prompt before insertion
(define-auto-insert "\.py" "module.py")
;; (define-auto-insert "\.php" "my-php-template.php")
;(add-hook 'find-file-hook 'auto-insert)

(setq auto-insert-alist
      '(
        ;; C++ Header
        (("\\.\\([H]\\|hh\\|hpp\\)\\'" . "C++ Header")
         nil
         "#if !defined(" (upcase (file-name-nondirectory buffer-file-name)) ")\n"
         "#define " (upcase (file-name-nondirectory buffer-file-name)) "\n\n"
         "#endif\n"
         )
        ;; Perl
        ((cperl-mode . "Perl Program")
          nil
          "#!/usr/bin/env perl\n#\n")
        ;; Shell
        ((sh-mode . "Shell script")
          nil
          "#!/bin/sh\n"
          "# -*- coding: UTF8 -*-\n\n")
        ))

(provide 'init-completion)