;; (require 'cc-mode)
;; À connaître dans le mode gdb :
;;- « M-x gdb » : activation du frontal graphique pour GDB (affichage
;;   multi-fenêtré car variable gdb-many-windows mise à 1)
;;- « M-x gdb-restore-windows » : restauration du mode multi-fenêtré après
;;   une bascule temporaire du tampon d'édition du code source pleine fenêtre.
;;- « M-x tool-bar-mode » : le frontal s'accompagne d'une barre d'outils
;;   adaptée qui peut s'avérer pratique.

(defun my-c-mode-common-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4
        tab-width 4
        c-brace-offset -4
        indent-tabs-mode nil
        c-auto-newline t ; va a la ligne quand on tape un ; ou {}
        )
  (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)

  ;; Chargement du mode fournissant une interface graphique à GDB
  (require 'gdb-ui)
  ;; Utilisation de GDB en mode multi-fenêtré, c'est quand même bien plus
  ;; puissant et pratique
  (setq gdb-many-windows 1)

  ;; Style d'indentation BSD
  ;; (setq c-indent-level 4)
  ;; (setq c-continued-statement-offset 4)
  ;; (setq c-argdecl-indent 0)
  ;; (setq c-label-offset -4)
)

(defun my-c-mode-hook ()
  (setq c-auto-hungry-initial-state 't)
  (setq c-delete-function 'backward-delete-char)
  (setq c-tab-always-indent 'nil)
  )

(defun my-c++-mode-hook ()
  (setq c++-auto-hungry-initial-state 't)
  (setq c++-delete-function 'backward-delete-char)
  (setq c++-tab-always-indent 'nil)
  (setq c++-empty-arglist-indent 4)
  )

(defun my-ctypes-mode-hook ()
  ;; ctypes permet d'ajouter à la liste des types de données C/C++ des types
  ;; non reconnus par défaut. Ces types sont alors connus et gérés par le
  ;; module de colorisation syntaxique.
  (require 'ctypes)
  (linum-mode 1)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-hook 'my-ctypes-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-ctypes-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; (defun my-ctypes-load-hook ()
;;   (ctypes-read-file "~/.emacs.d/ctypes" nil t t)
;; )
;; (add-hook 'ctypes-load-hook 'my-ctypes-load-hook)

(setq auto-mode-alist
      (append
       '(("\\.[ch]\\'" . c-mode)
         ("\\.[ch]pp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.hh\\'" . c++-mode)
         )
       auto-mode-alist))

(provide 'init-c)