;;; my-ghostel.el --- My extensions for ghostel

;;; Commentary:
;;

(defun my-ghostel-update-env (name)
  "Copy value env variable from Emacs into the current shell"
  (interactive (list (read-envvar-name "Update environment variable: " t )))
  (unless (derived-mode-p '(ghostel-mode))
    (error "current-buffer is not vterm buffer"))
  (let ((value (getenv name)))
    (ghostel-send-string (format "export %s=%s\n" name value))))

(provide 'my-ghostel)

;;; my-ghostel.el ends here
