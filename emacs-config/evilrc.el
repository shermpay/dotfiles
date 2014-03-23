;;; Sherman Pay Jing Hao
;;; Tuesday, 04. March 2014
;;; Evil mode customization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - - - - - - - - - Evil-mode - - - - - - - - -  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start EVIL!
(global-evil-leader-mode)
(evil-mode 1)

(setq evil-find-skip-newlines t)	;Able to skip new lines with f,F,t,T
(setq evil-move-cursor-back nil)	;Don't want to move cursor back when exit insert
;;; Relative linum
(require 'linum-relative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- HOOKS --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default State in Modes
(evil-set-initial-state 'cider-tmp 'emacs)
(evil-set-initial-state 'REPL 'emacs)
(evil-set-initial-state 'Inferior-python 'emacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Cursor --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-normal-state-cursor '("white" box))
(setq evil-emacs-state-cursor '("green" bar))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Mode line ---------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-mode-line-format '(before . mode-line-position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- TABS --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating tabs the vim way. Requires elscreen
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Rebinding Keys --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leader
(setq evil-leader/leader ",")
(evil-leader/set-key
  "f" 'ido-find-file
  )

;;; Other keys

;;; Mode switches
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
(define-key evil-insert-state-map [ctrl-j] 'evil-ret)
;;; Provide the config 
(provide 'evilrc)
