;;; package  --- Summary
;;; Commentary:
;;; Functions, Keybindings and random code snippets.


;;; Code:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
(define-key global-map (kbd "C-c t") 'eshell)
(define-key global-map (kbd "C-c i") 'imenu)

(provide 'my-stuff)
;;; my-stuff.el ends here
