;;; package  --- Summary
;;; Commentary:
;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; Functions that I defined and find really useful

;;; Code:
(require 'cl)
(require 's)
(require 'dash)
;;; ========================================
;;; ---------- Window/Buffer Functions
;;; ========================================
(defun kill-this-buffer-tab ()
  "Kill this buffer and deletes this tab."
  (interactive)
  (progn
    (kill-this-buffer)
    (elscreen-kill)))

;; (define-key evil-normal-state-map (kbd "C-w d") 'kill-this-buffer-tab) 

;;; ========================================
;;; ---------- EDITING Functions
;;; ========================================

;;; Paredit with Electric Return
 (defvar electrify-return-match
    "}"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches electrify-return-match then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun prog-mode-keys ()
  "Setup keybindings for 'prog-mode'."
  (local-set-key [(f11)] 'compile)
  (local-set-key (kbd "M-SPC") 'company-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------- EVIL MODE -----------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- C MODE --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-mode-font-lock-if0 (limit)
  "For when using if macro for comments"
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-hook ()
  (font-lock-add-keywords
    nil
    '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(defun my-c-mode-common-hook ()
  (progn
    (local-set-key (kbd "RET") 'electrify-return-if-match)
    (lambda () (electric-pair-mode 1))
    (c-set-offset 'case-label '+)))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;; ========================================
;;; ---------- JAVA MODE
;;; ========================================
(defun java-defer-loading ()
  (progn
    (defun java-compile (&rest args)
      "Compiles the current buffer with javac"
      (interactive)
      (let ((file (buffer-name)))
	(shell-command (concat "javac -g -Xlint " file))))

    (defun java-run (&rest args)
      "Runs the current buffer with java"
      (interactive)
      (let ((file (buffer-name)))
	(async-shell-command (concat "java -cp .:* " (substring file 0 (- (length file) 5))))))
    (define-key java-mode-map (kbd "C-c C-r") 'java-run)
    (define-key java-mode-map (kbd "C-c C-v") 'java-compile)))
(add-hook 'java-mode-hook 'java-defer-loading)
;;; ========================================
;;; ---------- LISP MODE
;;; ========================================
;;; Slime connect to remote lisp
(defvar *current-tramp-path* nil)
(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
	(lambda (f)
	  (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
	(lambda (f)
	  (substring f (length *current-tramp-path*))))
  (slime-connect "localhost" 4005))

;;; ========================================
;;; ---------- OCTAVE MODE
;;; ========================================
(defun octave-shell ()
  (interactive)
  (if (get-buffer "*Inferior Octave*")
      (switch-to-buffer-other-window "*Inferior Octave*")
    (run-octave)))

(eval-after-load "octave-mode"
  '(progn
     (define-key octave-mode-map (kbd "C-c C-z") 'octave-shell)))

;;;;;;;;;;;;;;;;;;;
;; GRAPHVIZ MODE ;;
;;;;;;;;;;;;;;;;;;;
(defun dot-to-png ()
  (interactive)
  (let* ((file (buffer-name))
	(out-file (substring file 0 (- (length file) 4))))
    (shell-command (concat "dot -Tpng " file " -o " out-file ".png"))))

;;; ========================================
;;; ---------- Term
;;; ========================================
(defun term-other-window ()
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn (split-window-sensibly (selected-window))
             (other-window 1)
             (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun new-term-other-window ()
  (interactive)
  (split-window-sensibly (selected-window))
  (other-window 1)
  (ansi-term (getenv "SHELL")))

(add-hook 'term-mode-hook
          (lambda () (define-key term-mode-map (kbd "C-S-c") #'term-interrupt-subjob)))


;;; ========================================
;;; ---------- MISC
;;; ========================================
(defun caesar-cipher (s n)
  (apply #'string
         (mapcar (lambda (x) (+ (mod (+ n (- x ?a)) 26) ?a)) (downcase s))))

(defun all-caesar-ciphers (s)
  (dotimes (n 26)
    (print (caesar-cipher s n))))

;;; reverse pairs in a list
(defun reverse-pairs (lst)
  "Reverse pairs in a list"
  (labels ((aux (lst result)
		(if (null lst)
		    result
		  (aux (rest (cdr lst)) (cons (-take 2 lst) result)))))
    (reduce (lambda (x acc) (append x acc)) (aux lst (list)))))

;;; Reverse char-pairs in a list
(defun reverse-char-pairs (beg end)
  "Reverse pairs of characters in a string"
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-min))))
  (progn
    (insert (concat (reverse-pairs
		     (string-to-list (buffer-substring-no-properties beg end)))))
    (delete-region beg end)))

;;; generates a list of characters from START to END
;;; Prepends/Appends a string if given
(defun* generate-characters (start end &key (prepend "") (append ""))
  (loop for x from (string-to-char start) to (string-to-char end)
	collect (concat prepend (char-to-string x) append)))

(defun gen-chars ()
  "Generates characters from START to END"
  (interactive)
  (let ((start (read-from-minibuffer "Start character: "))
	(end (read-from-minibuffer "End character: ")))
    (generate-characters start end)))

;; Add date capabilites
(defun insert-date (prefix)
  "Insert the current date. With p
prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "de_DE"))
    (insert (format-time-string format))))

(defun my-apply-function-to-region (from to function)
  "Apply an elisp function to region in FROM TO."
  (interactive "r\naFunction to apply: ")
  (princ (s-upper-camel-case (buffer-substring from to)) (current-buffer))
  (delete-region from to))

(defun my-upper-camel-case-word ()
  "Convert to upper camel-case from point to end of word."
  (interactive)
  (princ (s-upper-camel-case (thing-at-point 'symbol)) (current-buffer))
  (mark-sexp)
  (delete-region (point) (mark)))

(defun my-lower-camel-case-word ()
  "Convert to upper camel-case from point to end of word."
  (interactive)
  (princ (s-lower-camel-case (thing-at-point 'symbol)) (current-buffer))
  (mark-sexp)
  (delete-region (point) (mark)))

(defun my-clang-format-current-buffer ()
  "Run clang-format on the current buffer."
  (if (eq major-mode 'c++-mode)
      (let ((clang-fmt
             (apply #'concat
                    (-interpose " " (list *my-clang-format* *my-clang-format-args*))))
            (old-point (point)))
        (progn
          (shell-command-on-region (point-min) (point-max) clang-fmt
                                   :current-buffer :replace)
          (goto-char old-point)
          t))
    nil))

(provide 'my-functions)

;;; Info lookups

;;; my-functions.el ends here
