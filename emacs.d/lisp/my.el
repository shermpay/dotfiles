;;; my.el --- Core library functions for my usage  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

;;; Code:


;;; PROFILING

(defvar my-completing-read-start-time 0)
(defvar my-completing-read-profile-buffer)

(defun my-completing-read-profile-setup ()
  "."
  (setq my-completing-read-profile-buffer (get-buffer-create "*completing-read-profile*"))
  (advice-add 'completing-read-default :before #'my-completing-read-profile-before-advice)
  (advice-add 'completing-read-default :after #'my-completing-read-profile-after-advice))

(defun my-completing-read-profile-before-advice (prompt collection &rest _args)
  "Before Advice for to profile COMPLETING-READ.

See COMPLETING-READ for PROMPT and COLLECTION details."
  (condition-case nil
	  (progn
		(setq my-completing-read-start-time (current-time))
		(let ((collection-msg (cl-typecase collection
								(list (format "list %s... <%d items total>" (take 5 collection) (length collection)))
								(hash-table (format "hash-table %s... <%d items total>"
													(let (lst)
													  (maphash (lambda (k v) (push (cons k v) lst)) collection)
													  lst)
													(hash-table-count collection)))
								(obarray (format "obarray %s" collection))
								(function (format "function: %s" collection))
								(t (format "unknown collection type: %s" (type-of collection))))))
		  (with-current-buffer my-completing-read-profile-buffer
			(insert
			 (goto-char (point-max))
			 (format "completing-read at %s" (format-time-string "%H:%M:%S:%3Nms" my-completing-read-start-time))
			 ?\t "prompt: " prompt ?\n
			 ?\t "collection: " collection-msg ?\n))))
	(error nil)))


(defun my-completing-read-profile-after-advice (_prompt _collection &rest _args)
  "After Advice for to profile COMPLETING-READ.

See COMPLETING-READ for PROMPT and COLLECTION details."
  (unwind-protect
      (let* ((end-time (current-time))
             (end-time-str (format-time-string "%H:%M:%S:%3N" end-time))
             (elapsed (format-time-string "%s" (time-subtract end-time my-completing-read-start-time))))
        (with-current-buffer my-completing-read-profile-buffer
          (goto-char (point-max))
          (insert
           ?\t (format "end-time=%s; elapsed=%ss" end-time-str elapsed) ?\n ?\n)))
    (setq my-completing-read-start-time nil)))

(when nil
  (my-completing-read-profile-setup)
  (progn
	(advice-remove 'completing-read-default #'my-completing-read-profile-before-advice)
	(advice-remove 'completing-read-default #'my-completing-read-profile-after-advice)))


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

(defun my-reverse-pairs (list)
  "Reverse pairs in LIST."
  (cl-labels ((aux (list result)
		(if (null list)
		    result
		  (aux (cl-rest (cdr list)) (cons (take 2 list) result)))))
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


(provide 'my)
;;; my.el ends here
