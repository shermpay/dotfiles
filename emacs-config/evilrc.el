;;; Sherman Pay Jing Hao
;;; Tuesday, 04. March 2014
;;; Evil mode customization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - - - - - - - - - Evil-mode - - - - - - - - -  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start EVIL!
(global-evil-leader-mode)
(evil-mode 1)

;; (require 'surround)
;; (global-surround-mode 1)
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
;;; Dark Themed
;; (setq evil-normal-state-cursor '("white" box)
;;       evil-insert-state-cursor '("white" bar)
;;       evil-emacs-state-cursor '("green" bar))

(setq evil-normal-state-cursor '("dim gray" box)
      evil-insert-state-cursor '("dim gray" bar)
      evil-emacs-state-cursor '("blue" bar))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Mode line ---------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-mode-line-format '(before . mode-line-position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- TABS --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating tabs the vim way. Requires elscreen
(global-evil-tabs-mode)
;; (define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;create tab
;; (define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
;; (define-key evil-normal-state-map (kbd "C-w f") 'elscreen-find-file)  ;Create tab with new file
;; (define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
;; (define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- Rebinding Keys --------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful defaults
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "g j") 'evil-next-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "g k") 'evil-previous-line)

;;; Leader
(setq evil-leader/leader ",")
(evil-leader/set-key
  "f" 'ido-find-file
  "k" 'kill-this-buffer
  "g" 'elscreen-goto)

;;; Other keys

;;; Mode switches
(define-key evil-normal-state-map (kbd "C-\\") 'evil-emacs-state)
(define-key evil-insert-state-map (kbd "C-\\") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-\\") 'evil-exit-emacs-state)

;;; Key Translations
(define-key key-translation-map (kbd ",,") (kbd "C-x"))
(define-key key-translation-map (kbd ",q") (kbd "C-g"))
(define-key key-translation-map (kbd ",h") (kbd "C-h"))
(define-key key-translation-map (kbd ",p") (kbd "C-c p"))

;;; Ace jump
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)
;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))
 
(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))
 
(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))
;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; (define-key evil-normal-state-map [return] 'evil-ret-and-indent)
;; (define-key evil-insert-state-map [return] 'evil-ret-and-indent)
;; (define-key evil-insert-state-map [ctrl-j] 'evil-ret)
;;; Provide the config 
(provide 'evilrc)
