;; ^^^^^^^^^^^^^^^^^^^^ C MODE ^^^^^^^^^^^^^^^^^^^^
;; (require 'google-c-style)
(add-hook 'c-mode-hook (lambda ()
                         (c-set-style "linux" nil)))

(provide 'c)
;; <<<<<<<<<<<<<<<<<<<< END C >>>>>>>>>>>>>>>>>>>>
