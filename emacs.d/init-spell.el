;; M-x flyspell-mode      : suggestions de correction à la volée
;; M-x flyspell-prog-mode : idem mais adapté au développement (l'analyse se
;;                          limite alors aux commentaires et aux chaînes
;;                          littérales).
;; Dans ces modes, lorsque Aspell détecte un mot mal orthographié, pour faire
;; apparaître les suggestions, placer le curseur sur le mot et appuyer sur la
;; combinaison de touches « M-$ ».

;; Le correcteur Aspell est plus lent mais plus pertinent que Ispell. On
;; demande donc à Emacs d'utiliser Aspell dans le mode Ispell, normalement
;; chargé par défaut par Emacs 23. L'option « --sug-mode=fast » demande à
;; Aspell de limiter son analyse pour gagner en vitesse... au détriment de la
;; pertinence. Cette option est utile pour la correction orthographique à la
;; volée (le fonctionnement pouvant être encore accéléré en remplaçant
;; « fast » par « ultra ») mais je préfère la pertinence à la rapidité.


;; (setq ispell-extra-args '("--sug-mode=fast"))
(setq ispell-program-name "aspell")
(setq ispell-dictionary "francais")

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                haskell-mode-hook
                nxml-mode-hook
                perl-mode-hook
                java-mode-hook
                c-mode-hook
                c++-mode-hook
                erlang-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

;; (add-hook 'text-mode-hook 'flyspell-mode)

;; flyspell
;; (add-hook 'flyspell-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-k") 'flyspell-auto-correct-previous-word)
;;              ))

;; (defun fd-switch-dictionary()
;;   (interactive)
;;   (let* ((dic ispell-current-dictionary)
;;     	 (change (if (string= dic "deutsch8") "english" "deutsch8")))
;;     (ispell-change-dictionary change)
;;     (message "Dictionary switched from %s to %s" dic change)
;;     ))
;; (global-set-key (kbd "<f8>")   'fd-switch-dictionary)

(require 'auto-dictionary)
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

(provide 'init-spell)