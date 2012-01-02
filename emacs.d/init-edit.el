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
;; (auto-revert-mode 1)              ; detect changes on file
(global-auto-revert-mode 1)
;; (global-linum-mode 1)           ; display line numbers in margin (Emacs 23)
(setq kill-whole-line t)          ; kill-line including the line ending char
(setq next-line-add-newlines t)   ; C-n add new line

(setq-default indent-tabs-mode nil)  ; Indent with spaces
(setq c-basic-offset 2)              ; default indent: 2 spaces

;; enable some functions
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil) ; enable region-down(up)casing
(put 'upcase-region 'disabled nil)

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
;; ask to add a newline if missing
;; (setq require-final-newline 'query)

;; disable vc to speed up saves
;; (setq vc-handled-backends nil)

;; delete end-of-line spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; update year in copyright mention
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

(provide 'init-edit)
