;; ^^^^^^^^^^^^^^^^^^^^ C MODE ^^^^^^^^^^^^^^^^^^^^
(require 'google-c-style)

(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

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
