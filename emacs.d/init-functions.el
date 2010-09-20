;; M-x dos2unix/unix2dos - conversion des fins de ligne
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; Fonction remplaçant toutes les tabulations du tampon courant par le nombre
;; d'espaces qui ne modifie pas la mise en page apparente (étrangement, la
;; fonction native d'Emacs ne s'applique qu'à une région, pas à un tampon
;; entier).
(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max))
)

;; Effacement des caractères blancs (y compris les nouvelles lignes) jusqu'au
;; prochain caractère non blanc.
(defun trim-whitespace () (interactive)
  "Delete white-space characters up to the next non-white-space character."
  (save-excursion
    (if (re-search-forward "[  \t\r\n]*[^  \t\r\n]" nil t)
      (delete-region (match-beginning 0) (- (point) 1))
    )
  )
)

;; Suppression du formatage sur N colonnes (comme la fonction n'existe pas,
;; l'astuce consiste à définir temporairement une largeur de ligne extrêmement
;; grande et de formater le paragraphe sur cette base).
(defun remove-hard-wrap-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column 10000000))
    (fill-paragraph nil)
  )
)

;; Fonction équivalente à la précédente appliquée à la région sélectionnée et
;; non plus au paragraphe courant.
(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column 10000000))
    (fill-region start end)
  )
)

;; Insertion de date au format « AAAA-MM-JJ »
;; Fonction associée plus bas à la séquence « C-c d »
(defun insert-iso-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
)

;; Insertion de date en clair « JJ Mois AAAA »
;; Fonction associée plus bas à la séquence « C-c S-d »
(defun insert-text-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%d %B %Y"))
)

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
  (interactive)
  (my-mark)
  (comment-region (point) (mark)))
(defun my-uncomment-region-or-line ()
  (interactive)
  (my-mark)
  (uncomment-region (point) (mark)))

(provide 'init-functions)