;; c2c-mode.el --- Major mode to edit Camptocamp.org files in Emacs

;; Copyright (C) 2009-2011 Simon - saimon.org

;; Version: 0.2
;; Keywords: C2C major mode
;; Author: Saïmon <contact at saimon dot org>
;; URL: https://github.com/saimn/c2c-tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 675
;; Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; c2c-mode is a major mode for editing Camptocamp.org formatted textarea
;; (with a syntax based on markdown and BBcode) in GNU Emacs. c2c-mode is free
;; software, licensed under the GNU GPL.

;;; Installation:

;; Make sure to place `c2c-mode.el` somewhere in the load-path and add the
;; following lines to your `.emacs` file to associate c2c-mode with `.text`
;; files:
;;
;;     (autoload 'c2c-mode "c2c-mode.el"
;;        "Major mode for editing C2C files" t)
;;     (setq auto-mode-alist
;;        (cons '("camptocamp" . c2c-mode) auto-mode-alist))

;;; Customization:

;; the faces used for syntax highlighting can be modified to your liking by
;; issuing `M-x customize-group RET c2c-faces` or by using the "C2C Faces"
;; link at the bottom of the mode customization screen.

;;; Usage:

;; Keybindings are grouped by prefixes based on their function. For example,
;; commands dealing with headers begin with `C-c C-t`. The primary commands in
;; each group will are described below. You can obtain a list of all
;; keybindings by pressing `C-c C-h`.
;;
;;   * Anchors: `C-c C-a`
;;
;;     `C-c C-a l` inserts inline links of the form `[text](url)`.  If
;;     there is an active region, text in the region is used for the
;;     link text.  `C-c C-a w` acts similarly for wiki links of the
;;     form `[[WikiLink]]`.
;;
;;   * Images: `C-c C-i`
;;
;;     `C-c C-i i` inserts an image, using the active region (if any)
;;     as the alt text.
;;
;;   * Physical styles: `C-c C-p`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  `C-c C-p b` makes
;;     the selected text bold, `C-c C-p f` formats the region as
;;     fixed-width text, and `C-c C-p i` is used for italic text.
;;
;;   * Logical styles: `C-c C-s`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  Logical styles
;;     include blockquote (`C-c C-s b`), preformatted (`C-c C-s p`),
;;     code (`C-c C-s c`), emphasis (`C-c C-s e`), and strong (`C-c
;;     C-s s`).
;;
;;   * Headers: `C-c C-t`
;;
;;     All header commands use text in the active region, if any, as
;;     the header text.  To insert an atx or hash style level-n
;;     header, press `C-c C-t n` where n is between 1 and 6.  For a
;;     top-level setext or underline style header press `C-c C-t t`
;;     (mnemonic: title) and for a second-level underline-style header
;;     press `C-c C-t s` (mnemonic: section).
;;
;;   * Other commands
;;
;;     `C-c -` inserts a horizontal rule.
;;
;; Many of the commands described above behave differently depending on
;; whether Transient Mark mode is enabled or not.  When it makes sense,
;; if Transient Mark mode is on and a region is active, the command
;; applies to the text in the region (e.g., `C-c C-p b` makes the region
;; bold).  For users who prefer to work outside of Transient Mark mode,
;; in Emacs 22 it can be enabled temporarily by pressing `C-SPC C-SPC`.
;;
;; When applicable, commands that specifically act on the region even
;; outside of Transient Mark mode have the same keybinding as the with
;; the exception of an additional `C-` prefix.  For example,
;; `c2c-insert-blockquote` is bound to `C-c C-s b` and only acts on
;; the region in Transient Mark mode while `c2c-blockquote-region`
;; is bound to `C-c C-s C-b` and always applies to the region (when
;; nonempty).
;;
;; c2c-mode supports outline-minor-mode as well as org-mode-style
;; visibility cycling for atx- or hash-style headers.  There are two
;; types of visibility cycling: Pressing `S-TAB` cycles globally between
;; the table of contents view (headers only), outline view (top-level
;; headers only), and the full document view.  Pressing `TAB` while the
;; point is at a header will cycle through levels of visibility for the
;; subtree: completely folded, visible children, and fully visible.
;; Note that mixing hash and underline style headers will give undesired
;; results.

;;; Acknowledgments:

;; c2c-mode is based on markdown-mode from Jason Blevins
;; http://jblevins.org/projects/c2c-mode/


;;; Code:

(require 'easymenu)
(require 'outline)

;; A hook for users to run their own code when the mode is loaded.
(defvar c2c-mode-hook nil)


;;; Customizable variables ====================================================

(defvar c2c-hr-length 5)

(defvar c2c-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais"))

;;; Font lock =================================================================

(require 'font-lock)

(defvar c2c-keyword-face 'c2c-keyword-face
  "Face name to use for italic text.")

(defvar c2c-italic-face 'c2c-italic-face
  "Face name to use for italic text.")

(defvar c2c-bold-face 'c2c-bold-face
  "Face name to use for bold text.")

(defvar c2c-underline-face 'c2c-underline-face
  "Face name to use for underline text.")

(defvar c2c-header-face 'c2c-header-face
  "Face name to use as a base for headers.")

(defvar c2c-header-face-1 'c2c-header-face-1
  "Face name to use for level-1 headers.")

(defvar c2c-header-face-2 'c2c-header-face-2
  "Face name to use for level-2 headers.")

(defvar c2c-header-face-3 'c2c-header-face-3
  "Face name to use for level-3 headers.")

(defvar c2c-header-face-4 'c2c-header-face-4
  "Face name to use for level-4 headers.")

(defvar c2c-header-face-5 'c2c-header-face-5
  "Face name to use for level-5 headers.")

(defvar c2c-header-face-6 'c2c-header-face-6
  "Face name to use for level-6 headers.")

(defvar c2c-inline-code-face 'c2c-inline-code-face
  "Face name to use for inline code.")

(defvar c2c-list-face 'c2c-list-face
  "Face name to use for list markers.")

(defvar c2c-blockquote-face 'c2c-blockquote-face
  "Face name to use for blockquote.")

(defvar c2c-pre-face 'c2c-pre-face
  "Face name to use for preformatted text.")

(defvar c2c-link-face 'c2c-link-face
  "Face name to use for links.")

(defvar c2c-url-face 'c2c-url-face
  "Face name to use for URLs.")

(defvar c2c-link-title-face 'c2c-link-title-face
  "Face name to use for reference link titles.")

(defvar c2c-comment-face 'c2c-comment-face
  "Face name to use for HTML comments.")


(defgroup c2c-faces nil
  "Faces used in C2c Mode"
  :group 'c2c
  :group 'faces)

(defface c2c-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords."
  :group 'c2c-faces)

(defface c2c-italic-face
  '((t :inherit font-lock-variable-name-face :italic t))
  "Face for italic text."
  :group 'c2c-faces)

(defface c2c-bold-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for bold text."
  :group 'c2c-faces)

(defface c2c-underline-face
  '((t :inherit font-lock-variable-name-face :underline t))
  "Face for underline text."
  :group 'c2c-faces)

(defface c2c-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Base face for headers."
  :group 'c2c-faces)

(defface c2c-header-face-1
  '((t :inherit c2c-header-face :size "16"))
  "Face for level-1 headers."
  :group 'c2c-faces)

(defface c2c-header-face-2
  '((t :inherit c2c-header-face :size "14"))
  "Face for level-2 headers."
  :group 'c2c-faces)

(defface c2c-header-face-3
  '((t :inherit c2c-header-face :size "12"))
  "Face for level-3 headers."
  :group 'c2c-faces)

(defface c2c-header-face-4
  '((t :inherit c2c-header-face))
  "Face for level-4 headers."
  :group 'c2c-faces)

(defface c2c-header-face-5
  '((t :inherit c2c-header-face))
  "Face for level-5 headers."
  :group 'c2c-faces)

(defface c2c-header-face-6
  '((t :inherit c2c-header-face))
  "Face for level-6 headers."
  :group 'c2c-faces)

(defface c2c-inline-code-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'c2c-faces)

(defface c2c-list-face
  '((t :inherit font-lock-builtin-face))
  "Face for list item markers."
  :group 'c2c-faces)

(defface c2c-blockquote-face
  '((t :inherit font-lock-doc-face))
  "Face for blockquote sections."
  :group 'c2c-faces)

(defface c2c-pre-face
  '((t :inherit font-lock-constant-face))
  "Face for preformatted text."
  :group 'c2c-faces)

(defface c2c-link-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'c2c-faces)

(defface c2c-url-face
  '((t :inherit font-lock-string-face))
  "Face for URLs."
  :group 'c2c-faces)

(defface c2c-link-title-face
  '((t :inherit font-lock-comment-face))
  "Face for reference link titles."
  :group 'c2c-faces)

(defface c2c-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for HTML comments."
  :group 'c2c-faces)


(defconst c2c-regex-keyword
  "\\[\\/?\[a-zA-Z\]+\\]"
  ;"\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ].*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")

(defconst c2c-regex-link-inline
  "\\(\\[url\\(=.*?\\)?\\]\\)\\(.*?\\)\\(\\[\\/url\\]\\)"
  ;"\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for links: [url=link]text[/url].")

(defconst c2c-regex-img
  "\\(\\[img\\(=.*\\)?\\]\\)\\(.*?\\)\\(\\[\\/img\\]\\)"
  "Regular expression for links: [img=link]caption[/img].")

(defconst c2c-regex-color
  "\\(\\[color\\(=.*\\)?\\]\\)\\(.*?\\)\\(\\[\\/color\\]\\)"
  "Regular expression for links: [color=link]text[/color].")

(defconst c2c-regex-acronym
  "\\(\\[acronym\\(=.*\\)?\\]\\)\\(.*?\\)\\(\\[\\/acronym\\]\\)"
  "Regular expression for links: [acronym=link]text[/acronym].")

(defconst c2c-regex-header-1-atx
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst c2c-regex-header-2-atx
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst c2c-regex-header-3-atx
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst c2c-regex-header-4-atx
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst c2c-regex-header-5-atx
  "^\\(##### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst c2c-regex-header-6-atx
  "^\\(###### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst c2c-regex-header-1-setext
  "^\\(.*\\)\n\\(===+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst c2c-regex-header-2-setext
  "^\\(.*\\)\n\\(---+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst c2c-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching C2c horizontal rules.")

(defconst c2c-regex-code
  "\\(\\[c\\]\\)\\(.*?\\)\\(\\[\\/c\\]\\)"
  ;"\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ].*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")

(defconst c2c-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst c2c-regex-list
  "^[ \t]*\\([0-9]+\\.\\|[\\*\\+-]\\) "
  "Regular expression for matching list markers.")

(defconst c2c-regex-bold
  "\\(\\[b\\]\\)\\(.*?\\)\\(\\[\\/b\\]\\)"
  ;"\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst c2c-regex-italic
  "\\(\\[\\(i\\|q\\|quote\\)\\]\\)\\(.*?\\)\\(\\[\\/\\(i\\|q\\|quote\\)\\]\\)"
  ;"\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching italic text.")

(defconst c2c-regex-underline
  "\\(\\[\\(u\\|s\\)\\]\\)\\(.*?\\)\\(\\[\\/\\(u\\|s\\)\\]\\)"
  "Regular expression for matching underline text.")

(defconst c2c-regex-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst c2c-regex-line-break
  "  $"
  "Regular expression for matching line breaks.")

(defconst c2c-regex-wiki-link
  "\\[\\[\\(.*?\\)|\\(.*?\\)\\]\\]"
  "Regular expression for matching wiki links.")

(defconst c2c-regex-uri
  (concat
   "\\(" (mapconcat 'identity c2c-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst c2c-regex-angle-uri
  (concat
   "\\(<\\)\\("
   (mapconcat 'identity c2c-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst c2c-regex-email
  "\\(\\[email\\]\\)\\(.*?\\)\\(\\[\\/email\\]\\)"
  ;"<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst c2c-regex-list-indent
  "^\\(\\s *\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\(\\s +\\)"
  "Regular expression for matching indentation of list items.")

(defvar c2c-mode-font-lock-keywords-basic
  (list
;;   '(c2c-match-comments 0 c2c-comment-face t t)
   (cons c2c-regex-keyword 'c2c-keyword-face)
   (cons c2c-regex-code '(2 c2c-inline-code-face))
   (cons c2c-regex-pre 'c2c-pre-face)
   (cons c2c-regex-blockquote 'c2c-blockquote-face)
   (cons c2c-regex-header-1-setext 'c2c-header-face-1)
   (cons c2c-regex-header-2-setext 'c2c-header-face-2)
   (cons c2c-regex-header-1-atx 'c2c-header-face-1)
   (cons c2c-regex-header-2-atx 'c2c-header-face-2)
   (cons c2c-regex-header-3-atx 'c2c-header-face-3)
   (cons c2c-regex-header-4-atx 'c2c-header-face-4)
   (cons c2c-regex-header-5-atx 'c2c-header-face-5)
   (cons c2c-regex-header-6-atx 'c2c-header-face-6)
   (cons c2c-regex-hr 'c2c-header-face)
   (cons c2c-regex-list 'c2c-list-face)
   (cons c2c-regex-link-inline '((1 c2c-link-face t)
				 (2 c2c-url-face t)
				 (3 c2c-link-title-face t)
				 (4 c2c-link-face t)))
   (cons c2c-regex-img '((1 c2c-link-face t)
			 (2 c2c-url-face t)
			 (3 c2c-link-title-face t)
			 (4 c2c-link-face t)))
   (cons c2c-regex-color '((1 c2c-link-face t)
			   (2 c2c-inline-code-face t)
			   (3 c2c-link-title-face t)
			   (4 c2c-link-face t)))
   (cons c2c-regex-acronym '((1 c2c-link-face t)
			     (2 c2c-inline-code-face t)
			     (3 c2c-link-title-face t)
			     (4 c2c-link-face t)))
   (cons c2c-regex-wiki-link '((1 c2c-url-face t)
                               (2 c2c-link-title-face t)))
   (cons c2c-regex-bold '(2 c2c-bold-face))
   (cons c2c-regex-italic '(3 c2c-italic-face))
   (cons c2c-regex-underline '(3 c2c-underline-face))
   (cons c2c-regex-angle-uri 'c2c-link-face)
   (cons c2c-regex-uri 'c2c-link-face)
   (cons c2c-regex-email 'c2c-link-face)
   )
  "Syntax highlighting for C2c files.")

(defvar c2c-mode-font-lock-keywords
  (append
   c2c-mode-font-lock-keywords-basic)
  "Default highlighting expressions for C2c mode.")


;;; Syntax Table ==============================================================

(defvar c2c-mode-syntax-table
  (let ((c2c-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "w" c2c-mode-syntax-table)
    c2c-mode-syntax-table)
  "Syntax table for `c2c-mode'.")


;;; Element Insertion =========================================================

(defun c2c-wrap-or-insert (s1 s2)
 "Insert the strings S1 and S2.
If Transient Mark mode is on and a region is active, wrap the strings S1
and S2 around the region."
 (if (and transient-mark-mode mark-active)
     (let ((a (region-beginning)) (b (region-end)))
       (goto-char a)
       (insert s1)
       (goto-char (+ b (length s1)))
       (insert s2))
   (insert s1 s2)))

(defun c2c-insert-hr ()
  "Insert a horizonal rule."
  (interactive)
  (let (hr)
    (dotimes (count (- c2c-hr-length 1) hr)        ; Count to n - 1
      (setq hr (concat "* " hr)))                       ; Build HR string
    (setq hr (concat hr "*\n"))                         ; Add the n-th *
    (insert hr)))

(defun c2c-insert-bold ()
  "Insert markup for a bold word or phrase.
If Transient Mark mode is on and a region is active, it is made bold."
  (interactive)
  (c2c-wrap-or-insert "[b]" "[/b]")
  (backward-char 2))

(defun c2c-insert-italic ()
  "Insert markup for an italic word or phrase.
If Transient Mark mode is on and a region is active, it is made italic."
  (interactive)
  (c2c-wrap-or-insert "[i]" "[/i]")
  (backward-char 1))

(defun c2c-insert-underline ()
  "Insert markup for a underline word or phrase.
If Transient Mark mode is on and a region is active, it is made underline."
  (interactive)
  (c2c-wrap-or-insert "[u]" "[/u]")
  (backward-char 2))

(defun c2c-insert-code ()
  "Insert markup for an inline code fragment.
If Transient Mark mode is on and a region is active, it is marked
as inline code."
  (interactive)
  (c2c-wrap-or-insert "[c]" "[/c]")
  (backward-char 1))

(defun c2c-insert-link ()
  "Insert an inline link of the form [url=link]title[/url].
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (c2c-wrap-or-insert "[url]" "[/url]")
  ;(insert "()")
  (backward-char 1))

(defun c2c-insert-wiki-link ()
  "Insert a wiki link of the form [[url|title]].
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (c2c-wrap-or-insert "[[" "|]]")
  (backward-char 2))

(defun c2c-insert-image ()
  "Insert an inline image tag of the form [img=link]caption[/img].
If Transient Mark mode is on and a region is active, it is used
as the alt text of the image."
  (interactive)
  (c2c-wrap-or-insert "[img]" "[/img]")
  ;(insert "()")
  (backward-char 1))

(defun c2c-insert-header-1 ()
  "Insert a first level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (c2c-insert-header 1))

(defun c2c-insert-header-2 ()
  "Insert a second level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (c2c-insert-header 2))

(defun c2c-insert-header-3 ()
  "Insert a third level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (c2c-insert-header 3))

(defun c2c-insert-header-4 ()
  "Insert a fourth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (c2c-insert-header 4))

(defun c2c-insert-header-5 ()
  "Insert a fifth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (c2c-insert-header 5))

(defun c2c-insert-header-6 ()
  "Insert a sixth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (c2c-insert-header 6))

(defun c2c-insert-header (n)
  "Insert an atx-style (hash mark) header.
With no prefix argument, insert a level-1 header.  With prefix N,
insert a level-N header.  If Transient Mark mode is on and the
region is active, it is used as the header text."
  (interactive "p")
  (unless n                             ; Test to see if n is defined
    (setq n 1))                         ; Default to level 1 header
  (let (hdr hdrl hdrr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))      ; Build a hash mark header string
    (setq hdrl (concat hdr " "))
    (setq hdrr (concat " " hdr))
    (c2c-wrap-or-insert hdrl hdrr))
  (backward-char (+ 1 n)))

(defun c2c-insert-title ()
  "Insert a setext-style (underline) first level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "=" hdr)))  ; Build a === title underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n==========\n")
    (backward-char 12)))

(defun c2c-insert-section ()
  "Insert a setext-style (underline) second level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "-" hdr)))  ; Build a --- section underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n----------\n")
    (backward-char 12)))

(defun c2c-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (c2c-blockquote-region (region-beginning) (region-end))
    (insert "> ")))

(defun c2c-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.The characters PREFIX will appear at the beginning
of each line."
  (if mark-active
      (save-excursion
        (let ((endpos end))
          ; Ensure that there is a leading blank line
          (goto-char beg)
          (while (not (looking-back "\n\n" 2))
            (insert "\n")
            (setq endpos (+ 1 endpos)))
          ; Insert blockquote characters
          (move-to-left-margin)
          (while (< (point-at-bol) endpos)
            (insert prefix)
            (setq endpos (+ (length prefix) endpos))
            (forward-line))
          ; Move back before any blank lines at the end
          (goto-char endpos)
          (while (looking-back "\n" 1)
            (backward-char))
          ; Ensure one blank line at the end
          (while (not (looking-at "\n\n"))
            (insert "\n")
            (backward-char))))))

(defun c2c-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (c2c-block-region beg end "> "))

(defun c2c-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (c2c-pre-region (region-beginning) (region-end))
    (insert "    ")))

(defun c2c-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (c2c-block-region beg end "    "))

;;; Indentation ====================================================================

;;; Indentation functions contributed by Bryan Kyle <bryan.kyle@gmail.com>..

(defun c2c-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun c2c-prev-line-indent-p ()
  "Return t if the previous line is indented."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun c2c-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (when (re-search-forward "^\\s +" (point-at-eol) t)
        (current-column))))

(defun c2c-prev-list-indent ()
  "Return position of the first non-list-marker on the previous line."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (when (re-search-forward c2c-regex-list-indent (point-at-eol) t)
        (current-column))))

(defun c2c-indent-line ()
  "Indent the current line using some heuristics."
  (interactive)
  (if (c2c-prev-line-indent-p)
      ;; If the current column is any of the positions, remove all
      ;; of the positions up-to and including the current column
      (indent-line-to
       (c2c-indent-find-next-position
        (current-column) (c2c-calc-indents)))))

(defun c2c-calc-indents ()
  "Return a list of indentation columns to cycle through."
  (let (pos
        prev-line-pos
        positions
        computed-pos)

    ;; Previous line indent
    (setq prev-line-pos (c2c-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq positions (cons (c2c-prev-list-indent) positions))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward c2c-regex-list-indent (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if pos
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 (reverse positions)))

    positions))

(defun c2c-enter-key ()
  "Insert a newline and optionally indent the next line."
  (interactive)
  (newline)
  (funcall indent-line-function))


;;; Keymap ====================================================================

(defvar c2c-mode-map
  (let ((c2c-mode-map (make-keymap)))
    ;; Element insertion
    (define-key c2c-mode-map "\C-c\C-al" 'c2c-insert-link)
    (define-key c2c-mode-map "\C-c\C-aw" 'c2c-insert-wiki-link)
    (define-key c2c-mode-map "\C-c\C-ii" 'c2c-insert-image)
    (define-key c2c-mode-map "\C-c\C-t1" 'c2c-insert-header-1)
    (define-key c2c-mode-map "\C-c\C-t2" 'c2c-insert-header-2)
    (define-key c2c-mode-map "\C-c\C-t3" 'c2c-insert-header-3)
    (define-key c2c-mode-map "\C-c\C-t4" 'c2c-insert-header-4)
    (define-key c2c-mode-map "\C-c\C-t5" 'c2c-insert-header-5)
    (define-key c2c-mode-map "\C-c\C-t6" 'c2c-insert-header-6)
    (define-key c2c-mode-map "\C-c\C-pb" 'c2c-insert-bold)
    (define-key c2c-mode-map "\C-c\C-ss" 'c2c-insert-bold)
    (define-key c2c-mode-map "\C-c\C-pi" 'c2c-insert-italic)
    (define-key c2c-mode-map "\C-c\C-se" 'c2c-insert-italic)
    (define-key c2c-mode-map "\C-c\C-pu" 'c2c-insert-underline)
    (define-key c2c-mode-map "\C-c\C-pf" 'c2c-insert-code)
    (define-key c2c-mode-map "\C-c\C-sc" 'c2c-insert-code)
    (define-key c2c-mode-map "\C-c\C-sb" 'c2c-insert-blockquote)
    (define-key c2c-mode-map "\C-c\C-s\C-b" 'c2c-blockquote-region)
    (define-key c2c-mode-map "\C-c\C-sp" 'c2c-insert-pre)
    (define-key c2c-mode-map "\C-c\C-s\C-p" 'c2c-pre-region)
    (define-key c2c-mode-map "\C-c-" 'c2c-insert-hr)
    (define-key c2c-mode-map "\C-c\C-tt" 'c2c-insert-title)
    (define-key c2c-mode-map "\C-c\C-ts" 'c2c-insert-section)
    ;; Indentation
    (define-key c2c-mode-map "\C-m" 'c2c-enter-key)
    ;; Visibility cycling
    (define-key c2c-mode-map (kbd "<tab>") 'c2c-cycle)
    (define-key c2c-mode-map (kbd "<S-iso-lefttab>") 'c2c-shifttab)
    c2c-mode-map)
  "Keymap for C2c major mode.")

;;; Menu ==================================================================

(easy-menu-define c2c-mode-menu c2c-mode-map "Menu for C2C mode"
  '("C2C"
    ("Show/Hide"
     ["Cycle visibility" c2c-cycle (outline-on-heading-p)]
     ["Cycle global visibility" c2c-shifttab])
    "---"
    ("Headers (setext)"
     ["Insert Title" c2c-insert-title t]
     ["Insert Section" c2c-insert-section t])
    ("Headers (atx)"
     ["First level" c2c-insert-header-1]
     ["Second level" c2c-insert-header-2]
     ["Third level" c2c-insert-header-3]
     ["Fourth level" c2c-insert-header-4]
     ["Fifth level" c2c-insert-header-5]
     ["Sixth level" c2c-insert-header-6])
    "---"
    ["Bold" c2c-insert-bold]
    ["Italic" c2c-insert-italic]
    ["Underline" c2c-insert-underline]
    ["Blockquote" c2c-insert-blockquote]
    ["Preformatted" c2c-insert-pre]
    ["Code" c2c-insert-code]
    "---"
    ["Insert inline link" c2c-insert-link]
    ["Insert image" c2c-insert-image]
    ["Insert horizontal rule" c2c-insert-hr]
    ))


;;; Outline ===================================================================

;; The following visibility cycling code was taken from org-mode
;; by Carsten Dominik and adapted for c2c-mode.

(defvar c2c-cycle-global-status 1)
(defvar c2c-cycle-subtree-status nil)

;; Based on org-end-of-subtree from org.el
(defun c2c-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

;; Based on org-cycle from org.el.
(defun c2c-cycle (&optional arg)
  "Visibility cycling for C2c mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'."
  (interactive "P")
  (cond
     ((eq arg t) ;; Global cycling
      (cond
       ((and (eq last-command this-command)
             (eq c2c-cycle-global-status 2))
        ;; Move from overview to contents
        (hide-sublevels 1)
        (message "CONTENTS")
        (setq c2c-cycle-global-status 3))

       ((and (eq last-command this-command)
             (eq c2c-cycle-global-status 3))
        ;; Move from contents to all
        (show-all)
        (message "SHOW ALL")
        (setq c2c-cycle-global-status 1))

       (t
        ;; Defaults to overview
        (hide-body)
        (message "OVERVIEW")
        (setq c2c-cycle-global-status 2))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
        ;; Determine boundaries
        (save-excursion
          (outline-back-to-heading)
          (save-excursion
            (beginning-of-line 2)
            (while (and (not (eobp)) ;; this is like `next-line'
                        (get-char-property (1- (point)) 'invisible))
              (beginning-of-line 2)) (setq eol (point)))
          (outline-end-of-heading)   (setq eoh (point))
          (c2c-end-of-subtree t)
          (skip-chars-forward " \t\n")
          (beginning-of-line 1) ; in case this is an item
          (setq eos (1- (point))))
        ;; Find out what to do next and set `this-command'
      (cond
         ((= eos eoh)
          ;; Nothing is hidden behind this heading
          (message "EMPTY ENTRY")
          (setq c2c-cycle-subtree-status nil))
         ((>= eol eos)
          ;; Entire subtree is hidden in one line: open it
          (show-entry)
          (show-children)
          (message "CHILDREN")
          (setq c2c-cycle-subtree-status 'children))
         ((and (eq last-command this-command)
               (eq c2c-cycle-subtree-status 'children))
          ;; We just showed the children, now show everything.
          (show-subtree)
          (message "SUBTREE")
          (setq c2c-cycle-subtree-status 'subtree))
         (t
          ;; Default action: hide the subtree.
          (hide-subtree)
          (message "FOLDED")
          (setq c2c-cycle-subtree-status 'folded)))))

     (t
      (message "TAB")
      (funcall indent-line-function))))

;; Based on org-shifttab from org.el.
(defun c2c-shifttab ()
  "Global visibility cycling.
Calls `c2c-cycle' with argument t."
  (interactive)
  (c2c-cycle t))


;;; Miscellaneous =============================================================

(defun c2c-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun c2c-nobreak-p ()
  "Returns nil if it is ok for fill-paragraph to insert a line
  break at point"
  ;; are we inside in square brackets
  (looking-back "\\[[^]]*"))

;;; Mode definition  ==========================================================

(define-derived-mode c2c-mode text-mode "C2C"
  "Major mode for editing C2c files."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(c2c-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; For menu support in XEmacs
  (easy-menu-add c2c-mode-menu c2c-mode-map)
  ;; Make filling work with lists (unordered, ordered, and definition)
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t*][0-9]+\\.\\|^[ \t]*: ")
  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "#+")
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'c2c-nobreak-p)
  (setq indent-line-function 'c2c-indent-line))

;(add-to-list 'auto-mode-alist '("\\.text$" . c2c-mode))

(provide 'c2c-mode)

;;; c2c-mode.el ends here
