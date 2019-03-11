(require 'slime)
(require 'tramp)

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hok (lambda () (inferior-slime-mode t)))

(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
 
(slime-setup)
