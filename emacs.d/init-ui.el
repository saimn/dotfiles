;; (set-default-font "Inconsolata-10")
(setq font-use-system-font t)

;; Menubar, toolbar, scrollbar (set in ~/.Xdefaults)
;; (menu-bar-mode nil)
;; (tool-bar-mode nil)
;; (scroll-bar-mode nil)

;; Window title
;; (setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(setq frame-title-format '(buffer-file-name "%b [%f]" "%b"))

;; visible beep
(setq visible-bell t)
(setq cursor-type 'bar)

;; Choix de la police de caractères en environnement graphique
;; (if (eq window-system 'x)
;;   (progn
;;     ;; Je préfère voir apparaître le curseur sous la forme d'une barre
;;     ;; verticale clignotante.
;;     (bar-cursor-mode 1)
;;     (blink-cursor-mode 1)

;;     ;; Le menu est pratique lorsqu'on est en mode graphique mais encombrant et
;;     ;; moins utile dans une console texte.
;;     (menu-bar-mode 1)
;;   )
;;   ;; Le menu est pratique lorsqu'on est en mode graphique mais encombrant et
;;   ;; moins utile dans une console texte.
;;   (menu-bar-mode 0)
;; )

;; statusbar: column, line, time
(column-number-mode 1)
(line-number-mode 1)
(display-time-mode)                ; display time and load
;; (display-time)
;; (setq display-time-24hr-format t)

(defun my-toggle-menu-and-scrollbar ()
  "Toggles both menu-bar-mode and scroll-bar-mode"
  (interactive)
  (if tool-bar-mode
      (progn
        (tool-bar-mode -1)
        ;; (scroll-bar-mode -1)
        )
    (progn
      (tool-bar-mode 1)
      ;; (scroll-bar-mode 1)
      )))

;;------------------------------------------------------------
;; Speedbar
;;------------------------------------------------------------
;; (load-file "~/.emacs.d/sr-speedbar.el")
(require 'sr-speedbar)
(global-set-key (kbd "C-S-s") 'sr-speedbar-toggle)
;; (global-set-key [(super ?s)] 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)
;; permettre à la speedbar de reconnaître plus de fichiers
(speedbar-add-supported-extension "\\.\\(inc\\|txt\\)")
(speedbar-add-supported-extension "\\.\\(inc\\|ml[i]?\\)")
(speedbar-add-supported-extension "\\.\\(inc\\|xml\\)")
(speedbar-add-supported-extension "\\.\\(inc\\|css\\)")

(provide 'init-ui)