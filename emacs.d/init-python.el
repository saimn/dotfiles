;; (require 'python-mode)

(setq auto-mode-alist (cons '("\\.pyw?$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python" "Python editing mode." t)

;; allow inferior Python processes to load modules from the current directory
(setq python-remove-cwd-from-path 'nil)

;;----------------------------------------------------------------------
;; IPython
;;----------------------------------------------------------------------
;; load ipython.el if ipython is available
(when (executable-find "ipython")
  (require 'ipython nil 'noerror))
(when (featurep 'ipython)
  (setq python-python-command "ipython") ; -cl for classic prompt
  (setq python-command python-python-command)
  ;; (setq ipython-command "ipython")
  (setq py-python-command-args '( "-colors" "Linux"))
  (setq ipython-completion-command-string
        "print(';'.join(__IP.Completer.all_completions('%s')))\n")
  (autoload 'py-shell "ipython"
    "Use IPython as the Python interpreter." t))

;; Set PYTHONPATH
;; python-load-file not defined, may need to address this
(setenv "PYTHONPATH" ".")

;;----------------------------------------------------------------------
;; Fonctions
;;----------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(when (load "flymake" t)
  ;; (setq flymake-python-pyflakes-executable "pyflakes")
  ;; (load-library "flymake-cursor.el")

  ;; (add-hook 'python-mode-hook 'flymake-python-load)

  (defun flymake-pyflakes-init ()
    "Initialize flymake for python checking"
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  ;; use flymake for python for flymake-find-file-hook
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.pyw?\\'" flymake-pyflakes-init))

  (add-hook 'python-mode-hook
            (lambda ()
              (flymake-pyflakes-init)
              ;; prevent flymake from running over temporary interpreter buffers
              (unless (eq buffer-file-name nil) (flymake-mode 1))
              ;; flymake shortcuts
            (local-(setq  )et-key [f2] 'flymake-goto-prev-error)
            (local-set-key [f3] 'flymake-goto-next-error)
            )))

;;----------------------------------------------------------------------------

(setq eldoc-echo-area-use-multiline-p t ; eldoc may always resize echo area
      py-honour-comment-indentation t   ; comment indentation affects general indentation
      py-pychecker-command "pylint"     ; source code checking with pylint
      py-pychecker-command-args '("--output-format=parseable"))


(add-hook 'python-mode-hook
          (lambda ()
            (eldoc-mode t)
            ;; disable highlighting of unknown includes
            (semantic-toggle-decoration-style
             "semantic-decoration-on-includes" nil)
            ;; (define-key python-mode-map "\C-m"
            ;;   'python-reindent-then-newline-and-indent)
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace))))
            (set (make-local-variable 'tab-width) 4)
            (set (make-local-variable 'indent-tabs-mode) 'nil)))

(provide 'init-python)

