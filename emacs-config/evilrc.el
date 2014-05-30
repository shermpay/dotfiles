;;; Sherman Pay Jing Hao
;;; Tuesday, 04. March 2014
;;; Evil mode customization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - - - - - - - - - Evil-mode - - - - - - - - -  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start EVIL!
(global-evil-leader-mode)
(evil-mode 1)

(require 'surround)
(global-surround-mode 1)
(setq evil-move-cursor-back nil)	;Don't want to move cursor back when exit insert
;;; Relative linum
(require 'linum-relative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- HOOKS --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default State in Modes
(evil-set-initial-state 'cider-tmp 'emacs)
(evil-set-initial-state 'REPL 'emacs)
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
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;create tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-normal-state-map (kbd "C-w f") 'elscreen-find-file)  ;Create tab with new file
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Rebinding Keys --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leader
(setq evil-leader/leader ",")
(evil-leader/set-key
  "f" 'ido-find-file
  "k" 'kill-this-buffer
  "g" 'elscreen-goto
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

;; (define-key evil-normal-state-map [return] 'evil-ret-and-indent)
;; (define-key evil-insert-state-map [return] 'evil-ret-and-indent)
;; (define-key evil-insert-state-map [ctrl-j] 'evil-ret)
;;; Provide the config 
(provide 'evilrc)
