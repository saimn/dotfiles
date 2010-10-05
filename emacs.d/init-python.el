
;; (require 'python-mode)

(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq auto-mode-alist
      (append '(("\\.py$" . python-mode)
                ("SConstruct$" . python-mode)
                ("SConscript$" . python-mode))
              auto-mode-alist))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))


(setq ipython-command "ipython")
(require 'ipython)
;; (setq python-python-command "ipython")
(setq py-python-command-args '( "-colors" "Linux"))
(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")

(add-hook 'python-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (define-key python-mode-map "\C-m"
              'python-reindent-then-newline-and-indent)
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace))))
            (set (make-local-variable 'tab-width) 4)
            (set (make-local-variable 'indent-tabs-mode) 'nil)))

(defun python-reindent-then-newline-and-indent ()
  "Reindents the current line then creates an indented newline."
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (when (python-previous-line-is-comment)
      (insert "# "))
  (indent-according-to-mode))

(defun python-previous-line-is-comment ()
  "Returns `t' if the previous line is a Python comment."
  (save-excursion
    (forward-line -1)
    (python-line-is-comment)))

(defun python-line-is-comment ()
  "Returns `t' if the current line is a Python comment."
  (save-excursion
    (beginning-of-line)
    (search-forward "#" (point-at-eol) t)))

(provide 'init-python)


;;----------------------------------------------------------------------------
;; Pymacs and Rope for Python
;;----------------------------------------------------------------------------
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (require 'pymacs)
;;             (pymacs-load "ropemacs" "rope-")))

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
;'(flymake-allowed-file-name-masks (quote nil))

;; (setq flymake-python-pyflakes-executable "pyflakes")
;; (require 'flymake-python)
;; (load-library "flymake-cursor.el")

;; (add-hook 'python-mode-hook 'flymake-python-load)

