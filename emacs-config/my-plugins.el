;;; Sherman Pay Jing Hao
;;; Tuesday, 05. November 2013
;;; Contains all Major modes configuration

;;; exec path
;; (exec-path-from-shell-copy-env "PATH")
;;; Global Undo tree mode
(global-undo-tree-mode 1)

;;; Control mode
;; (control-mode-default-setup)

;;; elscreen
(elscreen-start)
(require 'elscreen-server)
;;; key chord modes
;; (require 'key-chord)
;; (key-chord-mode 1)
;; ;; (require 'space-chord)

;;; Ido modes
(flx-ido-mode 1)
(ido-ubiquitous 1)

;;; Projectile mode
(projectile-global-mode 1)
(setq projectile-mode-line (concat " Prj[" (projectile-project-name) "]"))

;;; Yassnippet
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt
			     yas-completing-prompt
			     yas-ido-prompt))
;;; Auto-complete modes
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "/home/shermpay/.emacs.d/elpa/auto-complete-20140208.653/dict")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 1)
(setq ac-trigger-commands nil)
;;; Adding ac-math (latex)

(defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
  (progn
    (require 'ac-math)
    (setq ac-sources
	 (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		 ac-sources))))
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(add-to-list 'ac-modes 'latex-mode)

(global-auto-complete-mode t)

;;; Jedi mode
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)

;;; Paredit-mode
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

;;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "ivory"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "snow1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orchid1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "bisque1"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "LightSteelBlue1")))) '(rainbow-delimiters-depth-6-face ((t (:foreground "dark gray"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "rosy brown"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "dark khaki"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "purple1"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "cadet blue")))))


;; ^^^^^^^^^^^^^^^^^^^^ Lisp / SLIME ^^^^^^^^^^^^^^^^^^^^
;; (setq slime-default-lisp 'clisp) ; Default lisp
;; (setq slime-lisp-implementations
;;       '((clisp ("/usr/local/bin/clisp" "-I"))
;; 	(sbcl ("/usr/bin/sbcl"))))
;; (require 'slime-autoloads)
;; (require 'slime-tramp)
 ;;; slime with tramp
;; (slime-setup '(slime-fancy))
;; <<<<<<<<<<<<<<<<<<<< END LISP >>>>>>>>>>>>>>>>>>>>

;; ^^^^^^^^^^^^^^^^^^^^ Clojure MODES ^^^^^^^^^^^^^^^^^^^^ 
;;; Cider
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)

(setq cider-auto-select-error-buffer t)  ;Auto select error buffer
(setq cider-repl-print-length 100)
(setq cider-stacktrace-fill-column 80)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)

;; <<<<<<<<<<<<<<<<<<<< END Clojure >>>>>>>>>>>>>>>>>>>>

;;; Scheme modes
;;; Racket
(add-hook 'racket-mode-hook (lambda () (geiser-mode 1)))
(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
;; ^^^^^^^^^^^^^^^^^^^^ WEB MODES ^^^^^^^^^^^^^^^^^^^^ 

;; <<<<<<<<<<<<<<<<<<<< END WEB >>>>>>>>>>>>>>>>>>>>

;; ^^^^^^^^^^^^^^^ Java MODE ^^^^^^^^^^^^^^^(add-hook 'java-mode-hook 'java-defer-loading)
;; (add-to-list 'load-path "~/.emacs.d/plugins/auto-java-complete/")
;;       (require 'ajc-java-complete-config)
      ;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)
      ;; (add-hook 'find-file-hook 'auto-4-jsp-find-file-hook)
;; <<<<<<<<<<<<<<< END JDE >>>>>>>>>>>>>>>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Groovy-mode           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set path to groovy binary
(require 'groovy-mode)
(setq groovy-home "/home/shermpay/.gvm/groovy/current")
(setq groovy-program-name "groovysh")
(add-to-list 'auto-mode-alist (cons "\\.gradle\\'" 'groovy-mode))
;;;;;;;;;;;;;;;;;;;;;
;;     Python Mode ;;
;;;;;;;;;;;;;;;;;;;;;
(setq python-indent-offset 4)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda () (electric-indent-mode 0)))

(setq python-shell-interpreter "ipython3"
       python-shell-interpreter-args ""
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code
       "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code
       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n" )
;; ^^^^^^^^^^^^^^^^^^^^ C MODE ^^^^^^^^^^^^^^^^^^^^
;; <<<<<<<<<<<<<<<<<<<< END C >>>>>>>>>>>>>>>>>>>>
(setq-default c-basic-offset 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --------- GO MODE ---------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(remove-hook 'before-save-hook 'gofmt-before-save)
;; ^^^^^^^^^^^^^^^^^^^^ OCTAVE MODE ^^^^^^^^^^^^^^^^^^^^
(setq inferior-octave-program "/usr/bin/octave")
(setq octave-block-offset 4)
(setq octave-block-comment-start "%%")
(setq octave-comment-char (string-to-char "%"))
(setq auto-mode-alist
      (cons '("\\.m\\'" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
	  (lambda ()
	    (define-key octave-mode-map (kbd "C-c C-z") 'octave-shell)))

;;;  ^^^^^^^^^^^^^^^^^^^^ LATEX MODE ^^^^^^^^^^^^^^^^^^^^
(load "auctex.el" nil t t)
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
	  (lambda () (set-face-foreground 'font-latex-math-face "chocolate")))

;; Org-mode
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) 
(add-hook 'org-mode-hook 'flyspell-mode)
;; (setq org-agenda-files (directory-files (concat *elisp-dir* "/org")))

;;;;;;;;;;;;;;
;; SML mode ;;
;;;;;;;;;;;;;;
(add-hook 'sml-mode-hook  (lambda () (clear-abbrev-table sml-mode-abbrev-table)))

;;;;;;;;;;;;;;;;;;;;
;; Compiler tools ;;
;;;;;;;;;;;;;;;;;;;;
(require 'flex-mode)
(require 'make-regexp)
(require 'bison-mode)
(provide 'my-plugins)
