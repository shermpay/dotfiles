;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; My Custom faces
;; *************** FACES ***************
(require 'noctilux-theme)
(setq theme-default 'noctilux-theme)
;;; Don't like all the fancy bars
(menu-bar-mode -1) 			
(menu-bar-no-scroll-bar)
(tool-bar-mode -1)
(bar-cursor-mode 1)

;;; Set the default colors, allows all frame to have the same colors
(setq default-frame-alist
      '((background-color . "gray8")
       ;; (foreground-color . "cyan2")
       ))

;; ;;; Default font
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 90)

;; ;;; Color Scheme
;; (set-face-foreground 'font-lock-function-name-face "red")
;; (set-face-foreground 'font-lock-keyword-face "yellow")
;; (set-face-foreground 'font-lock-type-face "RoyalBlue1")
;; (set-face-foreground 'font-lock-variable-name-face "GoldenRod1")
;; (set-face-foreground 'font-lock-builtin-face "orchid1")
;; (set-face-foreground 'font-lock-constant-face "green")
;; (set-face-foreground 'font-lock-comment-face "wheat")
;; (set-face-foreground 'font-lock-doc-face "wheat")
;; (set-face-foreground 'font-lock-string-face "LightPink")
;; (set-face-foreground 'font-lock-regexp-grouping-construct "DodgerBlue")
;; (set-face-foreground 'font-lock-regexp-grouping-backslash "DodgerBlue")
;; (set-face-foreground 'font-lock-negation-char-face "GoldenRod1")
;; (set-face-foreground 'minibuffer-prompt "SteelBlue1")
;; (set-face-foreground 'linum "azure3")

(require 'powerline)
(powerline-vim-theme)
(display-time-mode 1) 
 ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
 (set-frame-parameter (selected-frame) 'alpha '(50 20))
 (add-to-list 'default-frame-alist '(alpha 50 20))

(provide 'custom-faces)
(set-frame-parameter (selected-frame) 'alpha '(40 50))
