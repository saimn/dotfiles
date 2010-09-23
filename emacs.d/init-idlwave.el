
(defun my-idlwave-mode-hook ()
  (setq idlwave-system-directory "/opt/apps/rsi/idl71/")
  (setq idlwave-shell-explicit-file-name "/opt/apps/rsi/idl71/bin/idl")
  (setq idlwave-html-system-help-location "help/")

  ;; regular expressions matching special library directories for labeling
  ;; in routine-info display
  (setq idlwave-special-lib-alist
        '(("~/Dev/prior/trunk/EM/IDL/" . "PPrior")))

  ;; (hs-minor-mode)

  ;; completion for structures
  (setq idlw-complete-structtag t)

  ;(setq tab-width 4)
  (define-key idlwave-mode-map "\C-m" 'reindent-then-newline-and-indent)

  ;; Start autoloading routine info after 5 idle seconds
  (setq idlwave-init-rinfo-when-idle-after 5)

  ;; Pad operators with spaces
  (setq idlwave-do-actions t)
  (setq idlwave-surround-by-blank t)
  (lambda ()
    (idlwave-action-and-binding "*" '(idlwave-surround 1 1))
    (idlwave-action-and-binding "/" '(idlwave-surround 1 1))
    (idlwave-action-and-binding "-" '(idlwave-surround 1 1))
    (idlwave-action-and-binding "+" '(idlwave-surround 1 1)))

  ;; Remove spaces for keyword '='
  (setq idlwave-pad-keyword nil)

  ;; Syntax Highlighting
  (add-hook 'idlwave-mode-hook 'turn-on-font-lock)

  ;; Automatically start the shell when needed
  (setq idlwave-shell-automatic-start t)

  ;; point blinks to block beginning for idlwave-show-begin
  (setq idlwave-show-block t)

  ;; Leave ";" but not ";;" anchored at start of line
  (setq idlwave-begin-line-comment "^;[^;]")

  ;; uppercase for keywords
  ;(setq idlwave-reserved-word-upcase t)

  ;; indentation
  ;;(setq idlwave-main-block-indent 2)
  (setq idlwave-block-indent 3)
  (setq idlwave-end-offset -3)

  ;; C-S-b for setting a breakpoint, C-S-c for compiling the current source
  ;; file, C-S-a for deleting all breakpoints
  (setq idlwave-shell-debug-modifiers '(shift control))
  ;; or with SUPER key
  ;(setq idlwave-shell-debug-modifiers '(super))

  ;; shell in a dedicated frame
  ;(setq idlwave-shell-use-dedicated-frame t) -> use C-u C-c C-s instead
  ;(setq idlwave-shell-use-toolbar nil)           ; No toolbar
)

;; ;; First for the source buffer
;; (add-hook 'idlwave-mode-hook
;;        (lambda ()
;;          (local-set-key [f4] 'idlwave-shell-retall)
;;          (local-set-key [f5] 'idlwave-shell-break-here)
;;          (local-set-key [f6] 'idlwave-shell-clear-current-bp)
;;          (local-set-key [f7] 'idlwave-shell-cont)
;;          (local-set-key [f8] 'idlwave-shell-clear-all-bp)
;;          (local-set-key [kp-enter] 'idlwave-newline)))

;; ;; Then for the shell buffer
;; (add-hook 'idlwave-shell-mode-hook
;;        (lambda ()
;;          (local-set-key [f4] 'idlwave-shell-retall)
;;          (local-set-key [f5] 'idlwave-shell-break-here)
;;          (local-set-key [f6] 'idlwave-shell-clear-current-bp)
;;          (local-set-key [f7] 'idlwave-shell-cont)
;;          (local-set-key [f8] 'idlwave-shell-clear-all-bp)))

(add-hook 'idlwave-mode-hook 'my-idlwave-mode-hook)

(autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
(autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
(setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist))

(provide 'init-idlwave)