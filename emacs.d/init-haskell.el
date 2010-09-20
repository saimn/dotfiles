
;; (load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file.el")
;; (require 'haskell-mode)

(load-library "haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(provide 'init-haskell)