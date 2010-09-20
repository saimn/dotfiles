;; ctags path
(setq path-to-ctags "/usr/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name))
  )

;; (defun jds-find-tags-file ()
;;   "recursively searches each parent directory for a file named `tags' and returns the
;; path to that file or nil if a tags file is not found. Returns nil if the buffer is
;; not visiting a file"
;;   (labels
;;       ((find-tags-file-r (path)
;;          (let* ((parent (file-name-directory path))
;;                 (possible-tags-file (concat parent "tags")))
;;            (cond
;;              ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
;;              ((string= "/tags" possible-tags-file) (error "no tags file found"))
;;              (t (find-tags-file-r (directory-file-name parent)))))))

;;     (if (buffer-file-name)
;;         (catch 'found-it
;;           (find-tags-file-r (buffer-file-name)))
;;         (error "buffer is not visiting a file"))))

;; (defun jds-set-tags-file-path ()
;;   "calls `jds-find-tags-file' to recursively search up the directory tree to find
;; a file named `tags'. If found, calls `vtags-set-tags-file' with that path as an argument
;; otherwise raises an error."
;;   (interactive)
;;   (vtags-set-tagfile (jds-find-tags-file)))

(provide 'init-tags)