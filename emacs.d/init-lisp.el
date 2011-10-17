(defun my-lisp-mode-hook ()
  (define-key lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key lisp-mode-map "\C-i" 'lisp-indent-line)
  (define-key lisp-mode-map "\C-j" 'eval-print-last-sexp))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Compile .emacs
(defun compile-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    (byte-compile-file user-init-file)
    (message "Emacs init file saved and compiled.")))

(defun my-emacs-lisp-mode-hook ()
  ; rajouter ici le code souhaité, par exemple : (turn-on-eldoc-mode)
  (if (string-equal buffer-file-name (expand-file-name user-init-file))
      (progn (add-hook 'after-save-hook 'compile-init-file t t)
             )))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(setq auto-mode-alist
      (cons '("\\.el\\'" . emacs-lisp-mode) auto-mode-alist))
(autoload 'emacs-lisp-mode "emacs-lisp-mode" nil t)

(provide 'init-lisp)