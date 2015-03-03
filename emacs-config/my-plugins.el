;;; Sherman Pay Jing Hao
;;; Tuesday, 05. November 2013
;;; Contains all Major modes configuration

;;; Copy Environment Variables
;; (exec-path-from-shell-copy-env "PATH")
;; (exec-path-from-shell-copy-env "GOROOT")
;; (exec-path-from-shell-copy-env "GOPATH")
;;; Global Undo tree mode
(global-undo-tree-mode 1)
;;; elscreen
(setq elscreen-prefix-key (kbd "C-w"))
(elscreen-start)

;;; key chord modes
;; (require 'key-chord)
;; (key-chord-mode 1)
;; ;; (require 'space-chord)

;;; Projectile mode
(projectile-global-mode 1)
(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

;;; Helm modes
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) 
 ; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
 ; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-M-x-fuzzy-match t
      helm-quick-update t
      helm-split-window-in-side-p)
(helm-mode 1)

(require 'helm-projectile)

;;; Company modes
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;
;; Paredit-mode ;;
;;;;;;;;;;;;;;;;;;
(require 'paredit) 
(setq lisp-modes-hooks '(emacs-lisp-mode-hook
			 eval-expression-minibuffer-setup-hook
			 ielm-mode-hook
			 lisp-mode-hook
			 lisp-interaction-mode-hook
			 scheme-mode-hook
			 slime-mode-hook
			 clojure-mode-hook
			 cider-mode-hook))

(dolist (hook lisp-modes-hooks nil)
  (add-hook hook #'enable-paredit-mode))

;;;;;;;;;;;;
;; Octave ;;
;;;;;;;;;;;;
(defun require-octave ()
  (setq inferior-octave-program "/usr/bin/octave")
  (setq octave-block-offset 4)
  (setq octave-block-comment-start "%%")
  (setq octave-comment-char (string-to-char "%"))
  (setq auto-mode-alist
	(cons '("\\.m\\'" . octave-mode) auto-mode-alist))
  (add-hook 'octave-mode-hook
	    (lambda ()
	      (define-key octave-mode-map (kbd "C-c C-z") 'octave-shell))))

;;;  ^^^^^^^^^^^^^^^^^^^^ LATEX MODE ^^^^^^^^^^^^^^^^^^^^
(defun require-latex ()
  (load "preview-latex.el" nil t t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook (lambda () (progn
					  (auto-fill-mode 1)
					  (set-fill-column 80))))
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)			;Always compile to PDF
  (add-hook 'org-mode-hook
	    (lambda () (set-face-foreground 'font-latex-math-face "chocolate"))))

;; Org-mode
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) 
(add-hook 'org-mode-hook 'flyspell-mode)
(setq org-agenda-files (directory-files (concat *elisp-dir* "/org")))

;;;;;;;;;;;;;;
;; SML mode ;;
;;;;;;;;;;;;;;
(add-hook 'sml-mode-hook  (lambda () (clear-abbrev-table sml-mode-abbrev-table)))

;;;;;;;;;;;;;;;;;;
;; Haskell Mode ;;
;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;;;;;;;;;;;;;;;;;;;
;; Compiler tools ;;
;;;;;;;;;;;;;;;;;;;;
;; (require 'flex-mode)
;; (require 'make-regexp)
;; (require 'bison-mode)
(provide 'my-plugins)
 
