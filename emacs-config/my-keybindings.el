;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; My keybindings
;;; Overwrote certain default keybindings, and included keybindings for plugins

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - - - - - - - - - Global-mode - - - - - - - - - ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; magit
(global-set-key [(f12)] 'magit-status)
;;; My functions
(global-set-key (kbd "C-c d") 'insert-date) ;; Insert date command
;;; Overrides
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-5") 'query-replace-regexp)  ;Query replace regexp
(global-set-key (kbd "<f8>") 'tramp-cleanup-all-connections)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; Programming mode keys
(add-hook 'prog-mode-hook  'prog-mode-keys)
;;; Multiple cursors
(global-set-key (kbd "C-M-=") 'mc/edit-lines)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-+") 'mc/mark-all-like-this)

(global-set-key (kbd "M-`") 'er/expand-region) ;Expand Region

;;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)

;;; Term Mode
(global-set-key (kbd "<f10>") #'term-other-window)

;;; Elscreen
(global-set-key (kbd "M-1") (lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (elscreen-goto 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (elscreen-goto 4)))
(global-set-key (kbd "M-5") (lambda () (interactive) (elscreen-goto 5)))
(global-set-key (kbd "M-6") (lambda () (interactive) (elscreen-goto 6)))
(global-set-key (kbd "M-7") (lambda () (interactive) (elscreen-goto 7)))
(global-set-key (kbd "M-8") (lambda () (interactive) (elscreen-goto 8)))
(global-set-key (kbd "M-9") (lambda () (interactive) (elscreen-goto 9)))
(global-set-key (kbd "M-0") (lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "M-g w") #'elscreen-goto)
(global-set-key (kbd "M-g M-w") #'elscreen-goto)


;;; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Latex-math-mode
;; (define-key LaTeX-math-mode-map (kbd "` !") 'LaTeX-math-neg)
;; (define-key LaTeX-math-mode-map (kbd "` =") 'LaTeX-math-equiv)

;;; Key chords
;;(key-chord-define-global "vv" 'scroll-up-command)
;; (key-chord-define-global "rj" 'bookmark-jump)
;;(key-chord-define-global "qq" 'keyboard-quit)
;; (key-chord-define-global "kk" 'kill-this-buffer)
;;(key-chord-define-global "xb" 'ido-switch-buffer)
;;(key-chord-define-global "wb" 'ido-switch-buffer-other-window)
;;(key-chord-define-global "jw" 'other-window)
;;(key-chord-define-global "xx" 'save-buffer)
;;(key-chord-define-global "xv" 'eval-last-sexp)
;;(key-chord-define-global "jf" 'ido-find-file)
;;(key-chord-define-global "wf"  'ido-find-file-other-window)
;;(key-chord-define-global "rw" 'bookmark-jump-other-window)
;;;; (space-chord-define-global "j" 'ace-jump-char-mode)

;; (key-chord-define clojure-mode-map "EE" 'cider-eval-last-sexp)

(provide 'my-keybindings)
