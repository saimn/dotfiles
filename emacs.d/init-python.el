(autoload 'python "python" "Python editing mode." t)

;; allow inferior Python processes to load modules from the current directory
(setq python-remove-cwd-from-path 'nil)

;; Set PYTHONPATH
;; python-load-file not defined, may need to address this
(setenv "PYTHONPATH" ".")

;;----------------------------------------------------------------------
;; IPython
;;----------------------------------------------------------------------
;; load ipython.el if ipython is available
;; (when (executable-find "ipython2")
;;   (require 'ipython nil 'noerror))
;; (when (featurep 'ipython)
;;   (setq python-python-command "ipython2") ; -cl for classic prompt
;;   (setq python-command python-python-command)
;;   ;; (setq ipython-command "ipython")
;;   (setq py-python-command-args '( "-colors" "Linux"))
;;   (setq ipython-completion-command-string
;;         "print(';'.join(__IP.Completer.all_completions('%s')))\n")
;;   (autoload 'py-shell "ipython"
;;     "Use IPython as the Python interpreter." t)
;;   )

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(when (load "flymake" t)
  ;; (setq flymake-python-pyflakes-executable "pyflakes")
  (load-library "flymake-cursor.el")

  (defun flymake-pyflakes-init ()
    "Initialize flymake for python checking"
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  ;; use flymake for python for flymake-find-file-hook
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))

  (add-hook 'python-mode-hook
            (lambda ()
              (flymake-pyflakes-init)
              ;; prevent flymake from running over temporary interpreter buffers
              (unless (eq buffer-file-name nil) (flymake-mode 1))
              ;; flymake shortcuts
              (local-set-key [f10] 'flymake-goto-prev-error)
              (local-set-key [f11] 'flymake-goto-next-error)
              (local-set-key (kbd "C-c C-SPC") 'flymake-mode)
              (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
              (local-set-key (kbd "M-n") 'flymake-goto-next-error))))

;;----------------------------------------------------------------------------

(setq eldoc-echo-area-use-multiline-p t ; eldoc may always resize echo area
      py-honour-comment-indentation t   ; comment indentation affects general indentation
      py-pychecker-command "pylint"     ; source code checking with pylint
      py-pychecker-command-args '("--output-format=parseable"))

;; use pylint to check python files
(setq python-check-command "pep8 --repeat")

;;----------------------------------------------------------------------------

(add-hook 'python-mode-hook
          (lambda ()
            ;; disable highlighting of unknown includes
            ;; (semantic-toggle-decoration-style "semantic-decoration-on-includes" nil)

            (add-hook 'local-write-file-hooks 'untabify-buffer)

            ;; (add-hook 'local-write-file-hooks
            ;;           '(lambda()
            ;;              (save-excursion
            ;;                (untabify (point-min) (point-max))
            ;;                (delete-trailing-whitespace))))

            (eldoc-mode t)
            (show-paren-mode 1)
            (auto-fill-mode 1)
            (setq tab-width 4)
            (setq mode-name "py")

            ;; Keybindings
            (define-key python-mode-map "\C-ci" 'rope-auto-import)
            (define-key python-mode-map "\M-?" 'rope-code-assist)
            (define-key python-mode-map "\C-\M-?" 'rope-lucky-assist)
            (define-key python-mode-map "\C-cg" 'rope-goto-definition)
            (define-key python-mode-map "\C-cd" 'rope-show-doc)
            (define-key python-mode-map "\C-ct" 'rope-show-calltip)))

(provide 'init-python)

