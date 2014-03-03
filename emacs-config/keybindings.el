;;; Sherman Pay Jing Hao
;;; Tuesday, 17. December 2013
;;; My keybindings
;;; Overwrote certain default keybindings, and included keybindings for plugins

;;; Global-mode
;;; magit
(global-set-key (kbd "C-M-g") 'magit-status)
;;; My functions
(global-set-key (kbd "C-c d") 'insert-date) ;; Insert date command
;;; Overrides
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-6" ) 'delete-indentation)  ;Used to remove blank from one line to another(not between)
(global-set-key (kbd "M-5") 'query-replace-regexp)  ;Query replace regexp
(global-set-key (kbd "<f8>") 'tramp-cleanup-all-connections)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-g") 'goto-line)

(add-hook 'prog-mode-hook  'prog-mode-keys)
;;; Multiple cursors
(global-set-key (kbd "C-M-=") 'mc/edit-lines)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-+") 'mc/mark-all-like-this)

(global-set-key (kbd "M-2") 'er/expand-region) ;Expand Region
;;; Ace jump mode
(global-set-key (kbd "M-SPC") 'ace-jump-mode) 
(global-set-key (kbd "C-M-SPC") 'just-one-space)

;;; Hide Show mode
(global-set-key (kbd "M-RET") 'hs-toggle-hiding)

;;; Smex mode
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Yas bindings
(global-set-key [(control tab)] 'yas-expand)

;;; Key chords
(key-chord-define-global "vv" 'scroll-up-command)
(key-chord-define-global "rj" 'bookmark-jump)
(key-chord-define-global "qq" 'keyboard-quit)
(key-chord-define-global "kk" 'kill-this-buffer)
(key-chord-define-global "xb" 'ido-switch-buffer)
(key-chord-define-global "wb" 'ido-switch-buffer-other-window)
(key-chord-define-global "jw" 'other-window)
(key-chord-define-global "xx" 'save-buffer)
(key-chord-define-global "xv" 'eval-last-sexp)
(key-chord-define-global "jf" 'ido-find-file)
(key-chord-define-global "wf"  'ido-find-file-other-window)
(key-chord-define-global "rw" 'bookmark-jump-other-window)
;; (space-chord-define-global "j" 'ace-jump-char-mode)

;; (key-chord-define clojure-mode-map "EE" 'cider-eval-last-sexp)

(provide 'keybindings)
