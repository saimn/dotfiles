(defun dos2unix ()
  "Convert EOL to unix format"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  "Convert EOL to dos format"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; Replace tab by the corresponding number of spaces. Based on native emacs
;; function that works only on a region.
(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun trim-whitespace () (interactive)
  "Delete white-space characters up to the next non-white-space character."
  (save-excursion
    (if (re-search-forward "[  \t\r\n]*[^  \t\r\n]" nil t)
      (delete-region (match-beginning 0) (- (point) 1)))))

;; Suppression du formatage sur N colonnes (comme la fonction n'existe pas,
;; l'astuce consiste à définir temporairement une largeur de ligne extrêmement
;; grande et de formater le paragraphe sur cette base).
(defun remove-hard-wrap-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column 10000000))
    (fill-paragraph nil)))

;; Fonction équivalente à la précédente appliquée à la région sélectionnée et
;; non plus au paragraphe courant.
(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column 10000000))
    (fill-region start end)))

(defun insert-iso-date-string ()
  "Insert a numeric date string (year-month-day)."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-text-date-string ()
  "Insert a textual date string (day month year)."
  (interactive)
  (insert (format-time-string "%d %B %Y")))

(defun my-mark-line ()
  (let ((beg (point)))
    (if (= 0 (current-column))
        (forward-line 1)
      (end-of-line))
    (push-mark)
    (goto-char beg)))

(defun my-mark ()
  (if (not mark-active)
      (my-mark-line)))

(defun my-comment-region-or-line ()
  "Comment the region or line."
  (interactive)
  (my-mark)
  (comment-region (point) (mark)))

(defun my-uncomment-region-or-line ()
  "Uncomment the region or line."
  (interactive)
  (my-mark)
  (uncomment-region (point) (mark)))

(defun count-words ()
  "Print the number of words in the buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (re-search-forward "\\w+\\W*" (point-max) t))
        (setq count (1+ count)))
      (message "Buffer contains %d words" count))))

;; (defun word-count ()
;;   "Count words in buffer"
;;   (interactive)
;;   (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun goto-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(provide 'init-functions)