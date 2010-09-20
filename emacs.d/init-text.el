;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'imenu-add-menubar-index)
(add-hook ’text-mode-hook
           (lambda ()
             (auto-fill-mode 1)
             (setq default-justification ’full))
           )

;; don't cut before :
(defun my-fill-nobreak-predicate ()
  (save-match-data
    (or (looking-at "[ \t]*[])»!?;:]")
        (looking-at "[ \t]*\\.\\.\\.")
        (save-excursion
          (skip-chars-backward " \t")
          (backward-char 1)
          (looking-at "[([«]")))))

(setq fill-nobreak-predicate ’my-fill-nobreak-predicate)


;;------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode to edit Markdown files" t)

(setq markdown-enable-math t)

;; (defun markdown-custom ()
;;   "markdown-mode-hook"
;;   ;; Support SmartyPants.pl (installed as, say, /usr/local/bin/smartypants)
;;   ;; http://daringfireball.net/projects/smartypants/
;;   (setq markdown-command "markdown | smartypants"))
;; (add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

;; (add-hook 'markdown-mode-hook
;;        (let ((original-command (lookup-key markdown-mode-map [tab])))
;;             '(lambda ()
;;                (setq yas/fallback-behavior
;;                      '(apply ,original-command))
;;                (local-set-key [tab] 'yas/expand))))

(add-hook 'markdown-mode-hook '(lambda ()
                                 (setq yas/fallback-behavior
                                       '(apply 'original-command))
                                 (local-set-key [tab] 'yas/expand)))

;;------------------------------------------------------------
;; Other text modes
;;------------------------------------------------------------
(autoload 'c2c-mode "c2c-mode"
  "Major mode for editing with Camptocamp.org" t)

(autoload 'xbbcode-mode "xbbcode-mode"
  "Load xbbcode-mode for editing BBCode." t)

;; optional. Make the command easier to remember.
(defalias 'bbcode 'xbbcode-mode)

(setq auto-mode-alist
      (append '(("\\.md\\'" . markdown-mode)
                ("\\.mkd\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)
                ("\\.rst\\'" . rst-mode)
                ("vimperator" . xbbcode-mode)
                ("camptocamp" . c2c-mode)
                )
              auto-mode-alist))

(provide 'init-text)