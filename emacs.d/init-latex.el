;; AUCTeX and preview-latex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
;; C-c C-p C-b (M-x preview-buffer) : generate math/graphics previews
;; C-c C-p C-p (M-x preview-at-point) : refresh the preview.

(defun my-tex-mode-hook ()
  (setq TeX-auto-save t)
  (setq TeX-save-query nil) ;;autosave before compiling
  (setq TeX-parse-self t)
  (setq TeX-open-quote "«~")
  (setq TeX-close-quote "~»")
  (auto-fill-mode t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-plug-into-AUCTeX t)
  ;; generate PDF files using pdflatex by default
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-master nil))

(add-hook 'LaTeX-mode-hook 'my-tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq auto-mode-alist
      (append '(("\\.tex\\'" . latex-mode)
                ("\\.sty\\'" . latex-mode)
                ("\\.cls\\'" . latex-mode))
              auto-mode-alist))

(setq TeX-output-view-style (quote (
                                    ("^pdf$" "." "zathura %o")
                                    ("^ps$" "." "evince %o")
                                    ("^dvi$" "." "xdvi %o")
                                    )))
(setq tex-dvi-view-command "xdvi")
(setq tex-dvi-print-command "dvips")
(setq tex-alt-dvi-print-command "dvips")

(provide 'init-latex)
