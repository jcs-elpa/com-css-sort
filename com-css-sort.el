;;; com-css-sort.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-04-30 14:26:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Mimic Eclipse C-S-o key. (Organeize Imports)
;; Keyword: Common CSS Handy Sort Sorting
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (s "1.12.0"))
;; URL: https://github.com/jcs090218/organize-imports-java

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Common way of sorting the CSS attributes.
;;

;;; Code:

(require 'cl-extra)
(require 's)


(defvar com-css-sort-sort-type 0
  "Type of sorting CSS attributes.
0 : Sort by Type Group.
1 : Sort by Alphabetic Order.")

(defvar com-css-sort-attributes-order '("display"
                                        "position"
                                        "top"
                                        "right"
                                        "bottom"
                                        "left"
                                        "float"
                                        "clear"
                                        "visibility"
                                        "opacity"
                                        "z-index"
                                        "margin"
                                        "margin-top"
                                        "margin-right"
                                        "margin-bottom"
                                        "margin-left"
                                        "outline"
                                        "border"
                                        "border-top"
                                        "border-right"
                                        "border-bottom"
                                        "border-left"
                                        "border-width"
                                        "border-top-width"
                                        "border-right-width"
                                        "border-bottom-width"
                                        "border-left-width"
                                        "border-style"
                                        "border-top-style"
                                        "border-right-style"
                                        "border-bottom-style"
                                        "border-left-style"
                                        "border-color"
                                        "border-top-color"
                                        "border-right-color"
                                        "border-bottom-color"
                                        "border-left-color"
                                        "background"
                                        "background-color"
                                        "background-image"
                                        "background-repeat"
                                        "background-position"
                                        "cursor"
                                        "padding"
                                        "padding-top"
                                        "padding-right"
                                        "padding-bottom"
                                        "padding-left"
                                        "width"
                                        "min-width"
                                        "max-width"
                                        "height"
                                        "min-height"
                                        "max-height"
                                        "overflow"
                                        "list-style"
                                        "caption-side"
                                        "table-layout"
                                        "border-collapse"
                                        "border-spacing"
                                        "empty-cells"
                                        "vertical-align"
                                        "text-align"
                                        "text-indent"
                                        "text-transform"
                                        "text-decoration"
                                        "line-height"
                                        "word-spacing"
                                        "letter-spacing"
                                        "white-space"
                                        "color"
                                        "font"
                                        "font-family"
                                        "font-size"
                                        "font-weight"
                                        "content"
                                        "quotes")
  "List of CSS attributes sort order by type.")


;;;###autoload
(defun com-css-sort-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun com-css-sort-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun com-css-sort-back-to-indentation-or-beginning ()
  "Toggle between first character and beginning of line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun com-css-sort-get-current-line ()
  "Return the current line into string."
  (thing-at-point 'line t))

(defun com-css-sort-is-beginning-of-line-p ()
  "Is at the beginning of line?"
  (save-excursion
    (let ((current-point nil)
          (begin-line-point nil))
      (setq current-point (point))
      (beginning-of-line)
      (setq begin-line-point (point))
      (= begin-line-point current-point))))

;;;###autoload
(defun com-css-sort-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (interactive)
  (com-css-sort-back-to-indentation-or-beginning)
  (when (com-css-sort-is-beginning-of-line-p)
    (com-css-sort-back-to-indentation-or-beginning)))

;;;###autoload
(defun com-css-sort-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]\t]*$")))

;;;###autoload
(defun com-css-sort-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (interactive)
  (com-css-sort-back-to-indentation-or-beginning)
  (when (com-css-sort-is-beginning-of-line-p)
    (com-css-sort-back-to-indentation-or-beginning)))

(defun com-css-sort-is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

(defun com-css-sort-first-char-in-line-comment-line ()
  "Check if the first character in the current line the comment line."
  (com-css-sort-goto-first-char-in-line)
  (com-css-sort-is-inside-comment-block-p))

(defun com-css-sort-get-sort-list-until-empty-or-comment-line ()
  "Get the list we want to sort.
Depends on if we meet a empty line or a comment line."
  (save-excursion
    (let ((line-list '()))
      (while (not (and (com-css-sort-current-line-empty-p)
                       (com-css-sort-first-char-in-line-comment-line)))
        ;; Push the line into list.
        (push (com-css-sort-get-current-line) line-list)
        ;; Get next line of attribute.
        (forward-line 1))
      ;; Returns the list.
      line-list)))

;;;###autoload
(defun com-css-sort-is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (save-excursion
    (let ((current-point nil)
          (end-buffer-point nil))
      (setq current-point (point))
      (goto-char (point-max))
      (setq end-buffer-point (point))
      (= end-buffer-point current-point))))

;;;###autoload
(defun com-css-sort-next-blank-or-comment-line ()
  "Move to the next line containing nothing but whitespace or \
first character is a comment line."
  (interactive)
  (forward-line 1)
  (while (and (not (com-css-sort-current-line-empty-p))
              (not (com-css-sort-first-char-in-line-comment-line)))
    (forward-line 1)
    (end-of-line)))

(defun com-css-sort-beginning-of-attribute-block (current)
  "Get the beginning of the attribute block.
CURRENT : current point."
  (goto-char current)
  (search-backward "{")
  (forward-line 1)
  (beginning-of-line)
  (point))

(defun com-css-sort-end-of-attribute-block (current)
  "Get the end of the attribute block.
CURRENT : current point."
  (goto-char current)
  (re-search-forward "[{}]")
  (forward-line -1)
  (end-of-line)
  (point))

(defun com-css-sort-insert-line-list (line-list)
  "Insert list of line.
LINE-LIST : list of line."
  (save-excursion
    (dolist (line line-list)
      (insert line))))

;;;###autoload
(defun com-css-sort-attributes-block ()
  "Sort CSS attributes in the block."
  (interactive)
  (save-excursion
    ;; Ready to start sorting in the block.
    (let ((current (point))
          (start (css-sort-beginning-of-attribute-block current))
          (end (css-sort-end-of-attribute-block current)))

      ;; Goto beginning of the attribute block.
      (goto-char start)
      (forward-line 1)

      (while (< (point) end)
        ;; Get the empty/comment block of line list for next use.
        (let ((line-list (com-css-sort-get-sort-list-until-empty-or-comment-line))
              (end-region-point nil))
          ;; Get the current point again.
          (setq current (point))

          (when (>= (length line-list) 2)
            (save-excursion
              (com-css-sort-next-blank-or-comment-line)
              (setq end-region-point (point)))

            ;; Delete region.
            (delete-region current end-region-point)

            (cond (;; OPTION(jenchieh): Sort by Type Group.
                   (= com-css-sort-sort-type 0)
                   (progn
                     (setq line-list (sort line-list 'string<))
                     ))
                  (;; OPTION(jenchieh): Sort by Alphabetic Order.
                   (= com-css-sort-sort-type 1)
                   (progn
                     (setq line-list (sort line-list 'string<)))))

            ;; Insert the lines.
            (com-css-sort-insert-line-list line-list)))

        ;; Goto next blank line or comment line.
        (com-css-sort-next-blank-or-comment-line)))
    ))

;;;###autoload
(defun com-css-sort-attributes-document ()
  "Sort CSS attributes the whole documents."
  (interactive)
  (save-excursion
    )
  )

(provide 'com-css-sort)
;;; com-css-sort.el ends here
