;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; My Custom faces
;; *************** FACES ***************
(require 'noctilux-theme)
(if (not (display-graphic-p))
    (load-theme 'solarized-dark))

;;; Don't like all the fancy bars
(menu-bar-mode -1) 			
(menu-bar-no-scroll-bar)
(tool-bar-mode -1)
;; (bar-cursor-mode 1)

;;; Set the default colors, allows all frame to have the same colors
;; (setq default-frame-alist
;;       '((background-color . "gray8")
;;        ;; (foreground-color . "cyan2")
;;        ))

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
(set-face-foreground 'font-lock-comment-face "tan")
(set-face-foreground 'font-lock-comment-delimiter-face "tan")
;; (set-face-foreground 'font-lock-doc-face "wheat")
;; (set-face-foreground 'font-lock-string-face "LightPink")
;; (set-face-foreground 'font-lock-regexp-grouping-construct "DodgerBlue")
;; (set-face-foreground 'font-lock-regexp-grouping-backslash "DodgerBlue")
;; (set-face-foreground 'font-lock-negation-char-face "GoldenRod1")
;; (set-face-foreground 'minibuffer-prompt "SteelBlue1")
;; (set-face-foreground 'linum "azure3")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------- Mode-line ---------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-time-mode 1) 

;;;--------- Smart Mode line---------  
(setq sml/theme 'dark)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(sml/setup)
(dolist (mode '(" Undo-Tree" " Abbrev" " Paredit" " AC" " ARev" " yas"))
  (add-to-list 'sml/hidden-modes mode))
(dolist (dir '(("^~/Programming/" ":P:")
	       ("^~/Programming/Projects/" ":Prj:")
	       ("^~/.emacs.d/org" ":Org:")
	       ("^~/dotfiles/" ":dot:")))
 (add-to-list 'sml/replacer-regexp-list dir))

(setcdr (assq 'projectile-mode minor-mode-alist)
	(list (concat " Prj[" (projectile-project-name) "]")))

(provide 'my-faces)
