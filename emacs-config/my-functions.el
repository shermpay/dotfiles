;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; Functions that I defined and find really useful

(require 'cl)
;;; ========================================
;;; ---------- JUMPING Functions
;;; ========================================

;;; ========================================
;;; ---------- EDITING Functions
;;; ========================================

(defun copy-whole-line ()
  "Kills a whole line without remove it"
  (interactive)
  (let ((cur (point-marker))
	(start (progn
		 (move-beginning-of-line 1)
		 (point)))
	(end (progn
	       (move-end-of-line 1)
	       (point))))
    (kill-ring-save start end)
    (goto-char cur)
    (message "Saved %s in kill ring" (downcase (what-line)))))

;;; Paredit with Electric Return
 (defvar electrify-return-match
    "[\]}\)\"]"
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
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (local-set-key [(f11)] 'compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------- EVIL MODE -----------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- C MODE --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defvar *vergil-tramp-path*
  "/ssh:shermpay@vergil.u.washington.edu:")
 (defun slime-vergil ()
  (interactive)
  (connect-to-host *vergil-tramp-path*))

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


;;; ========================================
;;; ---------- MISC
;;; ========================================

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
  "Insert the current date. With prefix-argument, use ISO format. With two
prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "de_DE"))
    (insert (format-time-string format))))

(defun toggle-transparency ()
   (interactive)
   (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
       (set-frame-parameter nil 'alpha '(100 100))
     (set-frame-parameter nil 'alpha '(85 50))))

(provide 'my-functions)

;;; Info lookups

;;; my-functions.el ends here
