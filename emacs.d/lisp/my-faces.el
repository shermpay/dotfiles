;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; My Custom faces
;; *************** FACES ***************
(require 'color-theme)
(color-theme-initialize)


;;; Show which function I am in
(which-function-mode)

;;; Set the default colors, allows all frame to have the same colors
;; (setq default-frame-alist
;;       '((background-color . "gray8")
;;        ;; (foreground-color . "cyan2")
;;        ))

;; ;;; Default font
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 90)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------- Mode-line ---------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-time-mode 1) 

;;;--------- Smart Mode line---------  
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(sml/setup)
(dolist (mode '("overwrite-mode"))
  (add-to-list 'rm-whitelist mode))
(dolist (dir '(("^~/Programming/" ":P:")
	       ("^:DB:Projects/" ":Prj:")
	       ("^~/.emacs.d/org/" ":Org:")
	       ("^~/dotfiles/" ":Dot:")))
 (add-to-list 'sml/replacer-regexp-list dir t))

(setcdr (assq 'projectile-mode minor-mode-alist)
	(list (concat " {" (projectile-project-name) "}")))

(provide 'my-faces)
