;;------------------------------------------------------------
;; PHP
;;------------------------------------------------------------
;; Manuel php en français
(setq php-manual-url "http://fr.php.net/manual/fr/")
(autoload 'php-html-helper-mode "html-helper-mode" "html-helper-mode" t)
(autoload 'php-mode "php-mode" "PHP editing mode" t)
(add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")

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
(setq html-helper-use-expert-menu t)

;; Indenter automatiquement lorsque l'on appuie sur entrée
(defun my-html-helper-load-hook ()
  (define-key html-mode-map (kbd "RET") 'newline-and-indent))
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook)

;; Utiliser PSGML pour les fichiers SGML, HTML, XML
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;;------------------------------------------------------------
;; Css
;;------------------------------------------------------------
(require 'rainbow-mode)
(autoload 'css-mode "css-mode" "CSS editing mode" t)

(defun my-css-mode-hook ()
  (setq css-indent-offset 2)
  (define-key css-mode-map "\C-s" 'css-insert-section)
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
;; javascript
;;------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript-mode.el" nil t)

;;------------------------------------------------------------
(setq auto-mode-alist
      (append '(("\\.sgml\\'" . nxml-mode)
                ("\\.x[ms]l\\'" . nxml-mode)
                ("\\.[sx]?html?\\'" . nxml-mode)
                ("\\.tpl\\'" . nxml-mode)
                ("\\.php\\'" . php-mode)
                ("\\.inc\\'" . php-mode)
                ("\\.sql\\'" . sql-mode)
                ("\\.css\\'" . css-mode)
                )
              auto-mode-alist))

(provide 'init-html)