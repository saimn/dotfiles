;;------------------------------------------------------------
;; PHP
;;------------------------------------------------------------
;; Manuel php en français
(setq php-manual-url "http://fr.php.net/manual/fr/")

;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (require 'flymake-php)
;;             (flymake-mode t)))

(eval-after-load "php-mode"
  '(progn
     (define-key php-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;;------------------------------------------------------------
;; HTML
;;------------------------------------------------------------
;; Utiliser le menu expert
;; (setq html-helper-use-expert-menu t)

;; Indenter automatiquement lorsque l'on appuie sur entrée
(defun my-html-helper-load-hook ()
  (define-key html-mode-map (kbd "RET") 'newline-and-indent))
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook)

;;------------------------------------------------------------
;; Css
;;------------------------------------------------------------
(defun my-css-mode-hook ()
  (setq css-indent-offset 2)
  (define-key css-mode-map "\C-c s" 'css-insert-section)
  (rainbow-mode 1))

(add-hook 'css-mode-hook 'my-css-mode-hook)

(defun css-insert-section (section)
  "Inserts a css section."
  (interactive "sSection: ")
  (insert (concat "/*--[ " section " ]"))
  (while (< (point) (+ 70 (point-at-bol)))
    (insert "-"))
  (insert (concat "*/\n" "\n")))

;;------------------------------------------------------------
;; Utiliser PSGML pour les fichiers SGML, HTML, XML
;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;; (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(autoload 'rainbow-mode "rainbow-mode" "Highlight color names in buffer" t)
(autoload 'css-mode "css-mode" "CSS editing mode" t)
(autoload 'javascript-mode "javascript-mode.el" nil t)
(autoload 'php-html-helper-mode "html-helper-mode" "html-helper-mode" t)
(autoload 'php-mode "php-mode" "PHP editing mode" t)
;; (add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")

(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(autoload 'html-mumamo "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(autoload 'django-html-mumamo "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(autoload 'smarty-html-mumamo "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(autoload 'jinja-html-mumamo "jinja.el")
(require 'mumamo-fun)

(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 ;; mumamo-background-colors nil
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 nxml-degraded t)

(setq auto-mode-alist
      (append '(("\\.x[ms]l\\'" . nxml-mode)
                ("\\.[sx]?html?\\'" . html-mumamo)
                ("\\.tpl\\'" . smarty-html-mumamo)
                ("\\.php\\'" . html-mumamo)
                ("\\.inc\\'" . html-mumamo)
                ("\\.sql\\'" . sql-mode)
                ("\\.css\\'" . css-mode)
                ("\\.js\\'" . javascript-mode))
              auto-mode-alist))


;;------------------------------------------------------------
;; Commands
;;------------------------------------------------------------
(defun tidy-html ()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "tidy -i -w 120 -q"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Tidy Error Buffer*"
   ;; show error buffer?
   t))

(provide 'init-web)
