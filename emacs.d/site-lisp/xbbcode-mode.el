;-*- coding: utf-8 -*-
;; xbbcode-mode.el -- Major mode for editing bbcode.

;; Copyright © 2009 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: bbcode, php, lightweight markup

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License version 3, as published by the
;; Free Software Foundation.

;;; DESCRIPTION

;; A major mode for editing bbcode.
;; for download location and documentation, see:
;; http://xahlee.org/emacs/xbbcode-mode.html

;;; INSTALL

;; Open the file, then type Alt+x eval-buffer.
;; You are done.
;; When you need to edit BBCode, type Alt+x xbbcode-mode.

;; For more detail on automatic loading, please see the doc at home page.

;;; DOCUMENTATION

;; Full documentation is at: http://xahlee.org/emacs/xbbcode-mode.html

;; To see the inline documentation in emacs, type “C-h m”
;; (describe-mode). (if you have not load the mode, first type
;; Alt+x xbbcode-mode)

;;; HISTORY

;; version 1.2, 2010-06-20 Added xbbcode-show-bbcode-help. Improved menu items.
;; version 1.1, 2009-12-14 The xbbcode-about now has clickable links, and other minor improvement.
;; version 1.0, 2009-12-09 First version.

;;; Code:

(setq xbbcode-mode-version "1.2")

(defgroup xbbcode-mode nil
  "Major mode for editing bbcode."
  :group 'languages)

(defvar xbbcode-mode-hook nil "Standard hook for xbbcode-mode.")

(defvar xbbcode-mode-version nil "xbbcode-mode version string.")

(defvar xbbcode-mode-map nil "Keymap for xbbcode-mode")

(setq xbbcode-mode-map (make-sparse-keymap))
(define-key xbbcode-mode-map (kbd "C-c C-t") 'xbbcode-insert-tag)
(define-key xbbcode-mode-map (kbd "C-c C-h") 'xbbcode-show-bbcode-help)
(define-key xbbcode-mode-map [menu-bar] (make-sparse-keymap))
(let ((menuMap (make-sparse-keymap "BBCode")))
  (define-key xbbcode-mode-map [menu-bar xbbcode] (cons "BBCode" menuMap))
  (define-key menuMap [about] '("About xbbcode-mode..." . xbbcode-about))
  (define-key menuMap [xbbcode-insert-tag] '("Word to Tag" . xbbcode-insert-tag))
  (define-key menuMap [xbbcode-show-bbcode-help] '("Show common tags" . xbbcode-show-bbcode-help))
  )

;;; syntax table
(defvar xbbcode-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?= "." synTable)
    (modify-syntax-entry ?[ "." synTable)
    (modify-syntax-entry ?] "." synTable)
    synTable)
  "Syntax table for `xbbcode-mode'.")

;;; functions

(defun xbbcode-about ()
  "Describe the major mode."
  (interactive)
  (with-output-to-temp-buffer "*About xbbcode-mode*"
    (princ 
     (concat "Package name: xbbcode-mode\n"
             "Version: " xbbcode-mode-version "\n"
             "Author: Xah Lee\n"
             "To see inline documentation, call the command describe-mode while in the mode.\n\n"
             "Home page: URL `http://xahlee.org/emacs/xbbcode-mode.html' \n")
     )
    )
  )

(defun xbbcode-show-bbcode-help ()
  "Display commonly used bbcode tags."
  (interactive)
  (with-output-to-temp-buffer "*bbcode cheatsheet*"
    (princ 
"[b]bold[/b]
[i]italic[/i]
[u]underline[/u]
[s]strike-thru[/s]

[url]http://example.com/[/url]
[quote]something[/quote]
[img]http://example.com/cat.jpg[/img]
[email]mary@example.com[/email]

[quote=\"mary\"]i didn't do it[/quote]

[code]x = 5
print x
[/code]

[size=20]Features[/size]

[color=#FF0000]red[/color]
[color=FF0000]red[/color]
[color=red]red[/color]
[color=blue]blue[/color]

[color=red][size=20]Want to Save Money?[/size][/color]

[list]
[*]A list of items
[*]with bullets.
[/list]

[list=1]
[*]numbered first item
[*]second item
[/list]

[list=a]
[*]first item. Ordered by a, b, c etc.
[*]second tiem.
[/list]
"     )
    )
  )

(defun xbbcode-insert-tag ()
  "Insert a bbcode tag based on the word under cursor.

If cursor is on the word “url”, then it'll become “[url][/url]” with cursor
positioned in between the tags.

Other examples:

 [b]bold[/b]
 [i]italic[/i]
 [u]underline[/u]
 [s]strike-thru[/s]
 [code]x = 5[/code]
 [url]http://example.com/[/url]
 [quote]something[/quote]
 [img]http://example.org/x.jpg[/img]
 [email]me@example.com[/email]"
  (interactive)
  (let (bds p1 p2 myword)
    (setq bds (bounds-of-thing-at-point 'word))
    (setq p1 (car bds) p2 (cdr bds))
    (setq myword (buffer-substring-no-properties p1 p2))
    
    (delete-region p1 p2)
    (insert (concat "[" myword "]" "[/" myword "]"))
    (search-backward "[")))

;;; font-lock

(setq xbbcode-font-lock-keywords
      '(
        ("\\[b\\]\\|\\[i\\]\\|\\[u\\]\\|\\[s\\]\\|\\[url\\]\\|\\[img\\]\\|\\[code\\]\\|\\[quote\\]\\|\\[email\\]\\|\\[list\\]\\|\\[list=1\\]\\|\\[list=a\\]\\|\\[/b\\]\\|\\[/i\\]\\|\\[/u\\]\\|\\[/s\\]\\|\\[/url\\]\\|\\[/img\\]\\|\\[/code\\]\\|\\[/quote\\]\\|\\[/email\\]\\|\\[/list\\]\\|\\[/size\\]\\|\\[/color\\]\\|\\[\\*\\]\\|\\[color=[[:alpha:]]+\\]\\|\\[color=#?[0-9a-fA-F]\\{6\\}\\]\\|\\[size=[[:digit:]]+\\]" . font-lock-constant-face)
        ("\\[b\\]\\(.*?\\)\\[\\/b\\]" . (1 'bold))
        ("\\[i\\]\\(.*?\\)\\[\\/i\\]" . (1 'italic))
        ("\\[u\\]\\(.*?\\)\\[\\/u\\]" . (1 'underline))
        ("\\[s\\]\\(.*?\\)\\[\\/s\\]" . (1 'shadow)) ; for lack of predefined strick-thru face. Todo: need to define a strick-thru face.

        ("\\(\\[url=\\)\\(.*?\\)\\(\\]\\)" . (1 font-lock-constant-face)) ; coloring the “[url=”
        ("\\(\\[url=\\)\\(.*?\\)\\(\\]\\)" . (3 font-lock-constant-face)) ; coloring the “]”

        ("\\[url\\]\\(.*?\\)\\[\\/url\\]" . (1 'link)) ; coloring the body
        ("\\[url=\\(.*?\\)\\]\\(.*?\\)\\[\\/url\\]" . (1 'link)) ; coloring the link

        ("\\(\\[quote=\\)\\(.*?\\)\\(\\]\\)" . (1 font-lock-constant-face)) ; coloring the “[quote=”
        ("\\(\\[quote=\\)\\(.*?\\)\\(\\]\\)" . (3 font-lock-constant-face)) ; coloring the “]”

        ("\\[img\\]\\(.*?\\)\\[\\/img\\]" . (1 'link))
        ("\\[code\\]\\(.*?\\)\\[\\/code\\]" . (1 'fixed-pitch))
        )
      )

(defun xbbcode-mode ()
  "Major mode for editing BBCode.

Press “\\[xbbcode-insert-tag]” to change word under cursor into a tag.

Shortcuts             Command Name
\\[xbbcode-insert-tag]       `xbbcode-insert-tag'
\\[xbbcode-show-bbcode-help]       `xbbcode-show-bbcode-help'

Home page: URL `http://xahlee.org/emacs/xbbcode-mode.html'."
  (interactive)
  (kill-all-local-variables)
  
  (setq major-mode 'xbbcode-mode)
  (setq mode-name "BBCode")
  (set-syntax-table xbbcode-mode-syntax-table)
  (use-local-map xbbcode-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((xbbcode-font-lock-keywords) nil nil))

  (run-mode-hooks 'xbbcode-mode-hook))

(provide 'xbbcode-mode)
