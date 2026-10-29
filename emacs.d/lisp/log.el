;;; log.el --- Simple logging library

;;; Commentary:
;;

(require 'cl-lib)

;;; Code:

(defvar log-file (concat user-emacs-directory "log")
  "The default file to store logs.")

(defvar log--buffer nil
  "The buffer for logs, see LOG-INIT.")

(defun log-buffer ()
  "Return the buffer for log."
  (or log--buffer
      (error "log buffer not initialized; call LOG-INIT")))

(cl-defstruct (log--source (:constructor log--source-make))
  (file nil :type string)
  (line 0 :type integer)
  (column 0 :type integer))

(defun log--source-string (source)
  "Return the string representation of SOURCE."
  (format "%s:%d:%d"
          (log--source-file source)
          (log--source-line source)
          (log--source-column source)))

(defvar log-levels '((:error . 20)
                     (:warn . 15)
                     (:info . 10)
                     (:debug . 5))
  "An alist of (log-level-symbol . log-level-integer).")

(defun log--level-symbol-value (lvl)
  "Return the numeric value of LVL."
  (cdr-safe (assoc lvl log-levels)))

(defvar log-level :info
  "The level where LOGF will perform logging.")

(defun log--level-value ()
  "Return the numeric value of LOG-LEVEL."
  (log--level-symbol-value log-level))

(define-derived-mode log-buffer-mode special-mode "Log"
  "Major mode used in log buffer."
  ;; Make it easy to do like "tail -f".
  (setq-local window-point-insertion-type t))

(defun log-init ()
  "Initialize log."
  (setq log--buffer (find-file-noselect log-file
                                           nil ; NOWARN
                                           t ; RAWFILE
        ))
  (with-current-buffer log--buffer
    (read-only-mode)
    (log-buffer-mode)
    ))

(defun log-clear ()
  "Clear log."
  (interactive)
  (with-current-buffer log--buffer
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(defun log--internal (lvl source-string format-str &rest args)
  "Log formatted with ARGS applied to FORMAT-STR.

LVL is the log level symbol (see LOG-LEVELS).
SOURCE-STRING is a string representing the source location
 (see LOG--SOURCE-STRING)."
  (when (<= (log--level-value) (log--level-symbol-value lvl))
    (with-current-buffer log--buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert ?\[
                (format-time-string "%F %H:%M:%S" (current-time)) ?\s
                (format "%s" lvl) ?\s
                source-string
                ?\]
                (apply #'format format-str args)
                ?\n)))))


(defmacro logf (lvl format-str &rest args)
  "Log FORMAT-STR with ARGS.

LVL is the log level."
  `(log--internal ,lvl
                     ,(log--source-string
                       (log--source-make
                        :file (buffer-name)
                        :line (line-number-at-pos (point))
                        :column (current-column)))
                     ,format-str ,@args))


(provide 'log)

;;; log.el ends here
