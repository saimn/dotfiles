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

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

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

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

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

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


(provide 'init-functions)