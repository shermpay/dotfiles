;;; meow-config.el --- Configuration and utils for meow-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'cl-lib)

(require 'meow)

;;; Code:

(defun meow-config-validate-keys (key-config)
  "Check that KEY-CONFIG is valid."
  (let ((required-states '(normal motion leader)))
    (cl-flet*
        ((sort-symbols (symbols)
           (seq-sort (lambda (x y) (string< (symbol-name x) (symbol-name y)))
                     symbols))
         (validate-states ()
           (let ((states (thread-last key-config
                                      (seq-map #'car)
                                      (sort-symbols))))
             (unless (equal states (sort-symbols required-states))
               (user-error "meow-config-validate-keys: Invalid states in key-config; got=%s, want=%s"
                           states required-states))))
         (validate-normal-keys (normal-keybinds)
           (let* ((grouped-by-keys (seq-group-by #'car normal-keybinds))
                  (duplicate-keys (seq-filter (lambda (pair) (> (length (cdr pair)) 1)) grouped-by-keys)))
             (when duplicate-keys
               (user-error "Duplicate keys: %s" duplicate-keys)))))

      (validate-states)
      (validate-normal-keys (alist-get 'normal key-config)))))

(defun meow-config-list-unused-keys (key-config)
  "Return a list of characters for unused keys in KEY-CONFIG."
  (let ((keybinds (alist-get 'normal key-config))
        (available-keys (mapcar #'char-to-string (number-sequence ?\s ?~))))
    (seq-difference available-keys (mapcar #'car keybinds))))

(defun meow-config-keys (key-config)
  "Configure meow key bindings using KEY-CONFIG."
  (meow-config-validate-keys key-config)
  (dolist (state-config key-config)
    (let ((state (car state-config))
          (keys (cdr state-config)))
      (apply #'meow-define-keys
             state
             keys))))

(defvar meow-config-keys-file (concat user-emacs-directory "meow-keys.el"))

(defun meow-config--read-keys (&optional key-config-file)
  "Read key-config from KEY-CONFIG-FILE."
  (unless key-config-file
	(setq key-config-file meow-config-keys-file))
  (unless (file-exists-p key-config-file)
	(user-error "meow-config--read-keys: File '%s' does not exist" key-config-file))
  (with-temp-buffer
	(insert-file-contents key-config-file t)
	(read (current-buffer))))

(defvar meow-config--current-key-config
  nil
  "The current key config.
NOTE: changing this variable does not rebind the keys.")

(defun meow-config-load (&optional key-config-file)
  "Load meow-config from KEY-CONFIG-FILE.

If KEY-CONFIG-FILE is invalid, this will not update the current key-config."
  (interactive)
  (unless key-config-file
	(setq key-config-file meow-config-keys-file))
  (let ((new-key-config (meow-config--read-keys key-config-file)))
	(meow-config-keys new-key-config)
	(setq meow-config--current-key-config new-key-config))
  meow-config--current-key-config)

(defun meow-config-init ()
  "Load from `meow-config-keys-file' and enable `meow-global-mode'."
  (meow-config-load)
  (meow-global-mode))

(provide 'meow-config)

;;; meow-config.el ends here
