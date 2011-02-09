;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'imenu-add-menubar-index)
(add-hook 'text-mode-hook
           (lambda ()
             (auto-fill-mode 1)
             ;; (setq default-justification 'full)
             ))

;; don't cut before ponctuations
(add-hook 'fill-no-break-predicate 'fill-french-nobreak-p)

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
                                 (setq paragraph-start "\\*\\|$"
                                       paragraph-separate "$")
                                 (local-set-key [tab] 'yas/expand)))

;;------------------------------------------------------------
;; ReStructured Text
;;------------------------------------------------------------
;; autoload rst hook
(autoload 'rst-text-mode-bindings "rst")

;; ReStructuredText editing
(setq
 ;; no indentation in headlines
 rst-default-indent 0
 ;; headline decoration preference
 ;; (following the python documentation style)
 ;; rst-preferred-decorations '(;; # with overline for parts
 ;;                             (35 over-and-under 0)
 ;;                             ;; * with overline for chapters
 ;;                             (42 over-and-under 0)
 ;;                             ;; = for sections
 ;;                             (61 simple 0)
 ;;                             ;; - for subsections
 ;;                             (45 simple 0)
 ;;                             ;; ^ for subsubsections
 ;;                             (94 simple 0)
 ;;                             ;; " for paragraphs
 ;;                             (34 simple 0))
 )

;; auto filling
(add-hook 'rst-mode-hook 'auto-fill-mode)


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
