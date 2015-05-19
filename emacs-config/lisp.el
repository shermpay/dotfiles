;;;;;;;;;;
;; Lisp ;;
;;;;;;;;;;
;; (setq slime-default-lisp 'clisp) ; Default lisp
;; (setq slime-lisp-implementations
;;       '((clisp ("/usr/local/bin/clisp" "-I"))
;; 	(sbcl ("/usr/bin/sbcl"))))
;; (require 'slime-autoloads)
;; (require 'slime-tramp)
 ;;; slime with tramp
;; (slime-setup '(slime-fancy))

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;
;;; Cider
(add-hook 'clojure-mode-hook #'cider-mode)
;; (add-hook 'cider-mode-hook #'ac-cider-setup)
(defun clojure-indentation ()
  (put-clojure-indent 'match 'defun))
(add-hook 'clojure-mode-hook #'clojure-indentation)
(add-hook 'clojure-mode-hook #'flycheck-clojure-setup)
(setq cider-auto-select-error-buffer t)  ;Auto select error buffer
(setq cider-repl-print-length 100)
(setq cider-stacktrace-fill-column 80)

(add-hook 'clojure-mode-hook
	  (lambda () (define-key clojure-mode-map (kbd "C-c ?") #'clojure-cheatsheet)))


;;;;;;;;;;;;
;; Scheme ;;
;;;;;;;;;;;;
;;; Racket
;; (add-hook 'racket-mode-hook (lambda () (geiser-mode 1)))
;; (require 'ac-geiser)
;; (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;; (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'geiser-repl-mode))
(provide 'lisp)
