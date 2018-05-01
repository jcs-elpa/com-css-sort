;;; com-css-sort.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-04-30 14:26:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Mimic Eclipse C-S-o key. (Organeize Imports)
;; Keyword: Common CSS Handy Sort Sorting
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24.4") (s "1.12.0"))
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

(defun com-css-sort-is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

(defun com-css-sort-first-char-in-line-comment-line ()
  "Check if the first character in the current line the comment line."
  (save-excursion
    (com-css-sort-goto-first-char-in-line)
    (forward-char 1)
    (forward-char 1)
    (com-css-sort-is-inside-comment-block-p)))

(defun com-css-sort-get-sort-list-until-empty-or-comment-line ()
  "Get the list we want to sort.
Depends on if we meet a empty line or a comment line."
  (save-excursion
    (let ((line-list '()))
      (while (and (not (com-css-sort-current-line-empty-p))
                  (not (com-css-sort-first-char-in-line-comment-line)))
        (let ((current-line (com-css-sort-get-current-line)))
          (when (not (string-match "}" current-line))
            ;; Push the line into list.
            (push current-line line-list)))

        ;; Get next line of attribute.
        (forward-line 1))
      ;; Returns the list.
      line-list)))

;;;###autoload
(defun com-css-sort-next-blank-or-comment-line ()
  "Move to the next line containing nothing but whitespace or \
first character is a comment line."
  (interactive)
  (forward-line 1)
  (while (and (not (com-css-sort-current-line-empty-p))
              (not (com-css-sort-first-char-in-line-comment-line)))
    (forward-line 1)))

;;;###autoload
(defun com-css-sort-next-non-blank-or-comment-line ()
  "Move to the next line that is exactly the code.
Not the comment or empty line."
  (interactive)
  (forward-line 1)
  (while (and (or (com-css-sort-current-line-empty-p)
                  (com-css-sort-first-char-in-line-comment-line))
              (not (= (point) (point-max))))
    (forward-line 1)))

(defun com-css-sort-beginning-of-attribute-block (start)
  "Get the beginning of the attribute block.
START : current point."
  (goto-char start)
  (search-backward "{")
  (forward-line 1)
  (beginning-of-line)
  (point))

(defun com-css-sort-end-of-attribute-block (start)
  "Get the end of the attribute block.
START : current point."
  (goto-char start)
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

(defun com-css-sort-swap-list-element (lst index-a index-b)
  "Swap the element by using two index.
LST : list of array to do swap action.
INDEX-A : a index of the element.
INDEX-B : b index of the element."
  (rotatef (nth index-a lst) (nth index-b lst)))

(defun com-css-sort-sort-line-list-by-type-group (line-list)
  "Sort line list into type group order.
LINE-LIST : list of line."
  (let (;; List of index, corresponds to true value. (line)
        (index-list '())
        ;; Final return list.
        (return-line-list '()))

    (dolist (in-line line-list)
      (let ((index -1)
            (line-split-string '())
            (first-word-in-line ""))

        ;; Split the line to list.
        (setq line-split-string (split-string in-line ":"))

        ;; Get the type which is usually the first word.
        (setq first-word-in-line (nth 0 line-split-string))

        ;; Trim the white space.
        (setq first-word-in-line (string-trim first-word-in-line))

        (setq index (cl-position first-word-in-line
                                 com-css-sort-attributes-order
                                 :test 'string-match))

        ;; Add both index and line value to list.
        ;; Treat this as a `pair' data structure.
        (push index index-list)
        (push in-line return-line-list)))

    ;; Bubble sort the elements.
    (let ((index-i 0)
          (flag t))
      (while (and (< index-i (- (length index-list) 1))
                  (equal flag t))

        ;; Reset flag.
        (setq flag nil)

        (let ((index-j 0))
          (while (< index-j (- (- (length index-list) index-i) 1))

            (let ((index-a index-j)
                  (index-b (1+ index-j))
                  (value-a -1)
                  (value-b -1))
              (setq value-a (nth index-a index-list))
              (setq value-b (nth index-b index-list))

              (when (< value-b value-a)
                ;; Swap index.
                (com-css-sort-swap-list-element index-list
                                                index-b
                                                index-a)
                ;; Swap value with same index.
                ;; NOTE(jenchieh): we do this much is all because
                ;; of this line of code.
                (com-css-sort-swap-list-element return-line-list
                                                index-b
                                                index-a)

                ;; Set flag.
                (setq flag t)))

            ;; inc j.
            (setq index-j (1+ index-j))))
        ;; inc i.
        (setq index-i (1+ index-i))))

    ;; Return the sorted list.
    return-line-list))

;;;###autoload
(defun com-css-sort-attributes-block (&optional no-back-to-line)
  "Sort CSS attributes in the block.
NO-BACK-TO-LINE : Do not go back to the original line."
  (interactive)
  (let ((start-line-num (string-to-number (format-mode-line "%l"))))
    (save-excursion
      (save-window-excursion
        ;; Ready to start sorting in the block.
        (let ((current (point))
              (start (com-css-sort-beginning-of-attribute-block (point)))
              (end (com-css-sort-end-of-attribute-block (point))))
          ;; Goto beginning of the attribute block.
          (goto-char start)

          (while (< (point) end)
            ;; Get the empty/comment block of line list for next use.
            (let ((line-list (com-css-sort-get-sort-list-until-empty-or-comment-line))
                  (end-region-point nil))
              ;; Get the current point again.
              (setq current (point))

              (when (>= (length line-list) 2)
                (save-excursion
                  (com-css-sort-next-blank-or-comment-line)

                  ;; Find the last point
                  (let ((record-point (point)))
                    (when (com-css-sort-current-line-empty-p)
                      (forward-line -1)
                      (if (string-match "}" (com-css-sort-get-current-line))
                          (beginning-of-line)
                        ;; Back to point if not true.
                        (goto-char record-point))))
                  (setq end-region-point (point)))

                ;; Delete region.
                (delete-region current end-region-point)

                ;; NOTE(jenchieh): Design your sort algorithms here
                ;; depend on the type.
                (cond (;; OPTION(jenchieh): Sort by Type Group.
                       (= com-css-sort-sort-type 0)
                       (progn
                         (setq line-list (com-css-sort-sort-line-list-by-type-group line-list))))
                      (;; OPTION(jenchieh): Sort by Alphabetic Order.
                       (= com-css-sort-sort-type 1)
                       (progn
                         (setq line-list (sort line-list 'string<)))))

                ;; Insert the lines.
                (com-css-sort-insert-line-list line-list)))

            ;; Goto next blank line or comment line.
            (com-css-sort-next-blank-or-comment-line)

            ;; Then goto next code line.
            (com-css-sort-next-non-blank-or-comment-line)))))

    (when (equal no-back-to-line nil)
      (with-no-warnings
        (goto-line start-line-num))
      (end-of-line))))

;;;###autoload
(defun com-css-sort-attributes-document ()
  "Sort CSS attributes the whole documents."
  (interactive)
  (let ((start-line-num (string-to-number (format-mode-line "%l"))))
    (save-excursion
      (save-window-excursion
        (goto-char (point-min))

        (while (ignore-errors (search-forward "}"))
          ;; Sort once.
          (com-css-sort-attributes-block t)

          ;; Goto next blank line
          (com-css-sort-next-blank-or-comment-line))))

    ;; make sure go back to the starting line.
    (with-no-warnings
      (goto-line start-line-num))
    (end-of-line)))

(provide 'com-css-sort)
;;; com-css-sort.el ends here
