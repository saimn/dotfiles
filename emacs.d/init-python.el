
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

;; (require 'python-mode)
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

(defun my-python-mode-hook ()
  '(lambda () (eldoc-mode 1)) t
  )

;; (provide 'python-programming)

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Flymake
;'(flymake-allowed-file-name-masks (quote nil))

(provide 'init-python)


;; ;;----------------------------------------------------------------------------
;; ;; Pymacs and Rope for Python
;; ;;----------------------------------------------------------------------------
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (require 'pymacs)
;;             (pymacs-load "ropemacs" "rope-")))

;; ;;----------------------------------------------------------------------------
;; ;; On-the-fly syntax checking via flymake
;; ;;----------------------------------------------------------------------------
;; (setq flymake-python-pyflakes-executable "pyflakes")
;; (require 'flymake-python)
;; (load-library "flymake-cursor.el")

;; (add-hook 'python-mode-hook 'flymake-python-load)

