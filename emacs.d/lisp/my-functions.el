;;; my-functions.el --- Core library functions for my usage  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sherman Pay

;; Author: Sherman Pay <shermanpay1991@gmail.com>
;; Keywords: lisp, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'cl-lib)
(require 's)
(require 'dash)


;;; ========================================
;;; MISC
;;; ========================================
(defun my-dot-to-png ()
  "Generate a .png from the file of the current buffer.

Uses the `dot` tool."
  (interactive)
  (let* ((file (buffer-file-name))
		 (out-file (substring file 0 (- (length file) 4))))
    (shell-command (concat "dot -Tpng " file " -o " out-file ".png"))))

(defun my-caesar-cipher (s n)
  "Apply a caesar cipher to S using N as the offset."
  (apply #'string
         (mapcar (lambda (x) (+ (mod (+ n (- x ?a)) 26) ?a)) (downcase s))))

(defun my-reverse-pairs (list)
  "Reverse pairs in LIST."
  (cl-labels ((aux (list result)
		(if (null list)
		    result
		  (aux (cl-rest (cdr list)) (cons (-take 2 list) result)))))
    (cl-reduce (lambda (x acc) (append x acc)) (aux list (list)))))

(defun my-reverse-char-pairs (beg end)
  "Reverse pairs of characters in a string from BEG to END."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-min))))
  (progn
    (insert (concat (my-reverse-pairs
		     (string-to-list (buffer-substring-no-properties beg end)))))
    (delete-region beg end)))

(cl-defun my-generate-characters (start end &key (prepend "") (append ""))
  "Generate a list of characters from START to END.

Prepend the string with PREPEND and append to the string with APPEND."
  (cl-loop for x from (string-to-char start) to (string-to-char end)
		collect (concat prepend (char-to-string x) append)))

(defun my-gen-chars ()
  "Generate characters from START to END."
  (interactive)
  (let ((start (read-from-minibuffer "Start character: "))
	(end (read-from-minibuffer "End character: ")))
    (my-generate-characters start end)))

(defun my-insert-date (insert-date-format)
  "Insert the current date.

Prefix argument controls INSERT-DATE-FORMAT."
  (interactive "P")
  (let ((format (cond
				 ((null insert-date-format) "%d.%m.%Y")
				 ((equal insert-date-format '(4)) "%Y-%m-%d")
				 ((equal insert-date-format '(16)) "%A, %d. %B %Y")))
		(system-time-locale "de_DE"))
    (insert (format-time-string format))))


(defun my-apply-function-to-region (from to function)
  "Apply an elisp FUNCTION to region in FROM TO."
  (interactive "r\naFunction to apply: ")
  (princ (funcall function (buffer-substring from to)) (current-buffer))
  (delete-region from to))

(require 'json)
(defun my-json-to-csv ()
  "Convert JSON to CSV format."
  (let* ((tbl-json (json-read-file "/tmp/table.json"))
		 (tbl-lst (cl-map 'list (lambda (row-alist) (list (alist-get 'date row-alist) (alist-get 'count row-alist))) tbl-json))
		 (csv-lst (mapcar (lambda (row) (string-join row ",")) tbl-lst)))
	(string-join csv-lst "\n")))

(defconst my-conf-symbol-prefix "my-conf--")

(defun my-conf-add-to-list (list-symbol key value)
  "Add VALUE to LIST-SYMBOL overriding the VALUE mapped to KEY."

  (cl-check-type key symbol)

  (let* ((list-value (symbol-value list-symbol))
		 (conf-alist-symbol (intern (concat my-conf-symbol-prefix
											(symbol-name list-symbol))))
		 (conf-alist (if (boundp conf-alist-symbol)
						 (symbol-value conf-alist-symbol)
					   '()))
		 (original-pair (assoc key conf-alist))
		 (original-value (cdr original-pair)))
	(if original-pair
		(setcdr original-pair value)
	  (push (cons key value) conf-alist))

	(set conf-alist-symbol conf-alist)

	(if original-value
		(set list-symbol (delete original-value list-value)))

	(add-to-list list-symbol value)))


(provide 'my-functions)
;;; my-functions.el ends here
