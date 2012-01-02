;; (set-default-font "Inconsolata-10")
(setq font-use-system-font t)

;; redraw the display before process queued input events
;; see http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/
(setq redisplay-dont-pause t)

;; Window title
;; (setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(setq frame-title-format '(buffer-file-name "%b [%f]" "%b"))

;; visible beep
(setq visible-bell t)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; cursor
;; (setq cursor-type 'bar)
;; (blink-cursor-mode nil)
(blink-cursor-mode (- (*) (*) (*)))     ; don't blink !

;; x-window specific settings
;; (if (eq window-system 'x)
;;   (progn
;;     (menu-bar-mode 1)
;;   )
;;   (menu-bar-mode 0)
;; )

;; statusbar: column, line, time
(column-number-mode 1)
(line-number-mode 1)
(display-time-mode)                ; display time and load
;; (display-time)
;; (setq display-time-24hr-format t)

;; 'y' = yes, 'n' = no
(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-toggle-menu-and-scrollbar ()
  "Toggles both menu-bar-mode and scroll-bar-mode"
  (interactive)
  (if tool-bar-mode
      (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        )
    (progn
      (tool-bar-mode 1)
      (scroll-bar-mode 1)
      )))

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;;------------------------------------------------------------
;; Speedbar
;;------------------------------------------------------------
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images t)
;; Additional extensions we are interested in
(speedbar-add-supported-extension
  '("PKGBUILD" ".txt" ".org" ".pdf" ".css" ".tpl" "inc" ".php" ".js"
    ".conf" ".patch" ".diff" ".lua" ".sh"))

;; cedet
;; (global-ede-mode 1)
;; (require 'semantic/sb)
;; (semantic-mode 1)


(provide 'init-ui)