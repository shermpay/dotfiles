;;; Sherman Pay Jing Hao
;;; Tuesday, 04. March 2014
;;; Evil mode customization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - - - - - - - - - Evil-mode - - - - - - - - -  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start EVIL!
(evil-mode 1)
;;; Relative linum
(require 'linum-relative)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Mode line ---------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-mode-line-format '(before . mode-line-position))

;;; Creating tabs the vim way. Requires elscreen
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Rebinding Keys --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other keys

;;; Mode switches
(define-key evil-normal-state-map (kbd "C-M-z") 'evil-emacs-state)
(define-key evil-insert-state-map (kbd "C-M-z") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-M-z") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-z") 'keyboard-quit)
(define-key evil-insert-state-map (kbd "C-z") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-z") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-z") 'evil-normal-state)

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-normal-state-map [return] 'evil-ret-and-indent)
(define-key evil-insert-state-map [return] 'evil-ret-and-indent)
;;; Provide the config 
(provide 'evilrc)
