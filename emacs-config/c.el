;; ^^^^^^^^^^^^^^^^^^^^ C MODE ^^^^^^^^^^^^^^^^^^^^
(require 'google-c-style)

(require 'flymake)
(flymake-mode 1)
;;; flymake
(local-set-key (kbd "C-c f ]") 'flymake-goto-next-error)
(local-set-key (kbd "C-c C-f ]") 'flymake-goto-next-error)
(local-set-key (kbd "C-c f [") 'flymake-goto-prev-error)
(local-set-key (kbd "C-c C-f [") 'flymake-goto-prev-error)
(local-set-key (kbd "C-c C-f \\") 'flymake-display-err-menu-for-current-line)
(local-set-key (kbd "C-c f \\") 'flymake-display-err-menu-for-current-line)
(add-hook 'c-mode-hook (lambda ()
                         (c-set-style "linux" nil)))

(provide 'c)
;; <<<<<<<<<<<<<<<<<<<< END C >>>>>>>>>>>>>>>>>>>>
