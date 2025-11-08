;;; init.el -- Summary
;;; Commentary:
;;; Load my org babel config

;;; Code:

;;; Requires
(require 'cl-lib)

;;; Load path
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;;; User Info
(setq user-full-name "Sherman Pay"
	  user-mail-address "shermanpay1991@gmail.com")

;;; Basic Configuration
(setq debug-on-error t)
(setq inhibit-startup-message t)

(global-subword-mode t)
(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq next-line-add-newlines nil)
(save-place-mode 1)

(setq-default tab-width 4)
;;;; Buffers
(use-package uniquify
  :config
  ;; /foo/bar/name => name|/foo/bar
  (setq uniquify-buffer-name-style 'post-forward))
(use-package ibuffer
  :config
  (define-key global-map (kbd "C-x C-b") 'ibuffer)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (defun ibuffer-auto-mode-enable ()
	(ibuffer-auto-mode 1))
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode-enable))
(use-package midnight
  :config
  (midnight-mode 1)
  (midnight-delay-set 'midnight-delay "01:42am")
  (setq clean-buffer-list-delay-general 1))
;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))


;;;; Compilation
(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;;;; Backups
(let ((tmp (concat user-emacs-directory "tmp")))
  (setq backup-directory-alist
	`((".*" . ,tmp)))
  (setq auto-save-file-name-transforms
	`((".*" ,tmp t))))

;;;; UI
(column-number-mode t)
(require 'display-line-numbers)
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook #'display-line-numbers--turn-on)
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq echo-keystrokes 0.01)
(setq visible-bell t)

;;;; Emacs Server
(with-eval-after-load "server"
  (unless (server-running-p) (server-start)))

;;;; Emacs Desktop
(use-package emacs
  :config
  (desktop-save-mode t)
  (setq my-desktop-save-path "~/.emacs.d/desktops")
  (mkdir my-desktop-save-path :parents)
  (add-to-list 'desktop-path my-desktop-save-path))

;;;; OS
(setq shell-file-path "/bin/zsh")
(use-package dired
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  (put 'dired-find-alternate-file 'disabled nil))
(setq browse-url-generic-program (cl-ecase system-type
							   (gnu/linux "/usr/bin/google-chrome")
							   (darwin "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
      browse-url-browser-function 'browse-url-generic)

;;;; project.el

(setq project-vc-extra-root-markers '("MODULE.bazel" "go.mod" ".dir-locals.el"))
(setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)

;;;; tramp
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;;; Keybinds
(define-key global-map (kbd "M-o") 'other-window)

(define-key global-map (kbd "C-c r") #'recompile)

;;; Buffer/Window/Frame Management
(setopt switch-to-buffer-obey-display-actions t)
  
(setopt display-buffer-alist nil)

(setopt display-buffer-alist
		'(((derived-mode comint-mode compilation-mode vterm-mode)
		   (display-buffer-reuse-mode-window
			display-buffer-in-direction)
		   (inhibit-same-window . t)
		   (mode comint-mode compilation-mode vterm-mode vterm-copy-mode)
		   (direction . bottom))
		  ((derived-mode Info-mode help-mode helpful-mode)
		   (display-buffer-reuse-window
			display-buffer-in-side-window)
		   (inhibit-same-window . t)
		   (window-width . 0.33)
		   (mode Info-mode help-mode helpful-mode)
		   (side . right)
		   (slot . 0))
		  ))


;;;; Help/documentation sidebar
;;;; IBuffer/Imenu/dired sidebar?

;;; Package Management
(use-package package
  :config
  (require 'package)
  ;; (add-to-list 'package-archives
  ;;   '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
			   '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
			   '("elpa" . "https://elpa.gnu.org/packages/") t)
  (package-initialize))

;;;; Auto updates
(use-package auto-package-update
  :disabled
  :ensure nil
  :config
  (setq auto-package-update-interval 90)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

;;; Core Packages
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "")
  :config (global-undo-tree-mode 1)
  (setq my/undo-tree-history-dir (concat user-emacs-directory "undo-tree-history"))
  (make-directory my/undo-tree-history-dir t)
  (setq undo-tree-history-directory-alist `(("." . ,my/undo-tree-history-dir)))
  (defun my-evil-undo-system ()
	(evil-set-undo-system 'undo-tree))
  :hook
  (evil-mode . my-evil-undo-system))

(use-package evil
  :ensure t
  :init
  ;; (setq evil-want-keybinding nil)		; For evil-collection
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)
  (setq evil-normal-state-cursor '("dim gray" box)
		evil-insert-state-cursor '("dim gray" bar)
		evil-emacs-state-cursor '("dark violet" bar))
  (evil-set-undo-system 'undo-tree)

  (dolist (m '(dired-mode vterm-mode))
	(add-to-list 'evil-emacs-state-modes m)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list 'exec-path-from-shell-variables "MY_DRIVE")
  (add-to-list 'exec-path-from-shell-variables "INCLUDEDIR")
  (exec-path-from-shell-initialize))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;; (defun crm-indicator (args)
  ;;   (cons (format "[CRM%s] %s"
  ;;                 (replace-regexp-in-string
  ;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;                  crm-separator)
  ;;                 (car args))
  ;;         (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  ;; (setq minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  ;; (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p));; Enable vertico

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setopt register-preview-delay 0.5
		  register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (add-to-list 'consult-buffer-filter "vterm\s+.*")

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ; (setopt consult-preview-key 'any)
  (setopt consult-preview-key '(:debounce 0.3 any))
  ;; (setopt consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil))
  )


;; TODO: Migrate to completion-preview-mode
(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; (setq text-mode-ispell-word-completion nil)
  )

;;;; Navigation
(use-package avy
  :config
  (avy-setup-default)
  (define-key evil-motion-state-map (kbd "SPC") #'avy-goto-word-or-subword-1)
  (setq avy-all-windows nil
		avy-all-windows-alt t))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-scope 'frame))

;;; Programming Modes
;;;; Checkers
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
(setq help-at-pt-timer 0.9)
(setq help-at-pt-display-when-idle '(flymake-overlay))

;; Spell checkers
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
(cond ((executable-find "enchant-2")  (setq-default ispell-program-name "enchant-2"))
	  ((executable-find "hunspell")   (progn (setq-default ispell-program-name "hunspell") (setq ispell-really-hunspell t)))
	  ((executable-find "aspell")     (setq-default ispell-program-name "aspell")))

;;;; LSP
(use-package eglot)

;;;; Treesitter
(use-package treesit
  :config
  (setq treesit-language-source-alist
		'((c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.21.0"))
		  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5"))
		  (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
		  (cmake . ("https://github.com/uyha/tree-sitter-cmake" "v0.5.0")))))
 
;;;; Lisp
;;;;; Common Lisp
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(setq flycheck-emacs-lisp-load-path 'inherit)
;;;;; Clojure
(use-package clojure-mode
  :ensure nil
  :hook
  (clojure-mode . (lambda () (require 'display-fill-column-indicator) (display-fill-column-indicator--turn-on))))

(use-package flycheck-clj-kondo)

(use-package flycheck-mode
  :hook clojure-mode)

;;;;; Lisp Shared
(use-package lispy
  :ensure nil
  :if (package-installed-p 'lispy)
  :hook
  (((emacs-lisp-mode lisp-mode clojure-mode scheme-mode) . lispy-mode))
  :config
  (lispy-set-key-theme '(special c-digits paredit))
  (define-key lispy-mode-map-paredit (kbd "M-o") nil)
  (define-key lispy-mode-map-paredit (kbd "M-[") #'lispy-wrap-brackets)
  (define-key lispy-mode-map-paredit (kbd "M-{") #'lispy-wrap-braces))

(use-package lispyville
  :ensure nil
  :if (package-installed-p 'lispyville)
  :hook
  (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
	 c-w
	 wrap
	 slurp/barf-lispy
	 additional
	 additional-motions)))


;;;; C/C++

;;;; Go
(defun add-hook-gofmt-before-save ()
  (add-hook 'before-save-hook 'gofmt-before-save nil t))
(use-package go-mode
  :ensure nil
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . add-hook-gofmt-before-save)
  :config (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/golang.org/x/lint/misc/emacs/")))

;;;; OCaml
(use-package tuareg
  :ensure nil)
;; (use-package ocamlformat
;;   :ensure nil
;;   :config
;;   (add-hook 'tuareg-mode-hook (lambda ()
;; 				;; (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
;; 				(add-hook 'before-save-hook #'ocamlformat-before-save))))
;;; Additional Packages
;;;; Magit
(use-package magit
  :ensure nil
  :pin melpa)

;;;; Terminal
(use-package vterm
  :ensure nil
  :config
  (setq vterm-buffer-name-string "vterm [%s]")
  (setq vterm-timer-delay 0.01)
  (setq vterm-max-scrollback 100000)
  (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (define-key global-map (kbd "C-c t") #'vterm)
  (require 'my-vterm)
  :hook (vterm-mode . (lambda () (goto-address-mode 1))))

;;;; Evil Collection for non-fundamental buffers
;; (use-package evil-collection
;;   :ensure nil
;;   :after evil
;;   :diminish (evil-collection-unimpaired-mode . "")
;;   :config
;;   (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
;;   (evil-collection-init))

;;;; Toggle between the different case types (ie. CamelCase, underscore_case, kebab-case).
;; (use-package string-inflection
;;   :ensure nil)

;;;; Protobufs/Bazel
(use-package protobuf-mode
  :ensure nil
  :mode "\\.proto")
(use-package bazel
  :disabled
  :ensure nil
  :pin melpa
  :mode "BUILD")

;;;; Highlight Indentation Levels
(use-package highlight-indent-guides
  :ensure nil
  :if (package-installed-p 'highlight-indent-guides)
  :config
  (setq highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))


(require 'org)

;;;; Helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
		 ("C-h v" . helpful-variable)
		 ("C-h k" . helpful-key)
		 ("C-h x" . helpful-command))
  :config
  (defun my-helpful--reference-positions (orig-fn &rest args)
	;; This speeds up helpful, when the symbol in question is in a large file.
	'())

  (advice-add 'helpful--reference-positions :around #'my-helpful--reference-positions)
  )
;;; Org Mode
;;;; The following are builtin configurations. 
(setq org-hide-leading-stars t)
(setq org-adapt-indentation t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(defun my-gdrive-or-emacs-dir ()
  (if (getenv "MY_DRIVE")
      (file-name-as-directory (getenv "MY_DRIVE"))
    (do
     (message "Missing 'MY_DRIVE' environment variable!")
     user-emacs-directory)))
;;;; Variables
(use-package org
  :config
  (setq org-directory (concat (my-gdrive-or-emacs-dir) "org/"))
  (defconst my/org-agenda-directory (concat org-directory "agenda/"))
  (defconst my/org-notes-directory (concat org-directory "notes/"))
  (defconst my/org-projects-directory (concat org-directory "projects/"))
  (defconst my/org-recurring-directory (concat org-directory "recurring/"))
  (setq org-agenda-files (list my/org-agenda-directory my/org-projects-directory my/org-recurring-directory))
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (defconst my/org-todo-file (concat my/org-agenda-directory "todo.org"))
  (setq org-todo-keywords '((sequence "TODO(t)" "WORKING(w)" "BLOCKED(b)" "IN_REVIEW(r)" "VERIFY(v)"
									  "|" "DONE(d)" "OBSOLETE(o)" "BACKLOGGED(B)" )))
  (setq org-enforce-todo-dependencies t)
  (setq org-refile-targets
		'((nil :maxlevel . 3)
		  (org-agenda-files :maxlevel . 3)))
  (setq org-id-method 'ts)			   ; use timestamp
  ;; Create an ID if needed to make a link.
  (setq org-id-link-to-org-use-id t)
  (setq org-list-allow-alphabetical t)
  (org-element-update-syntax)				; this is needed for the above

  ;; Clocking
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp t)
	 (dot . t)))
  )

;;;; Configure minor modes to enable/disable for org-mode
(defun my-org-mode-config-minor-modes ()
  ;; disable
  (with-eval-after-load 'flycheck (flycheck-mode -1))
  ;; enable
  (visual-line-mode))
(add-hook 'org-mode-hook #'my-org-mode-config-minor-modes)
;;;; Plugins/Modules
(with-eval-after-load "org"
  (nconc org-modules
		 '(
		   org-tempo
		   org-capture
		   org-protocol
		   ;; org-habit
		   ;; org-id
		   ;; org-brain
		   ))
  (org-load-modules-maybe t))
(use-package org-bullets
  :if (package-installed-p 'org-bullets)
  :ensure nil
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;;;; Notifications
(appt-activate)				; Builtin appt package for notifications
(setq appt-message-warning-time 6)
(setq appt-display-duration 30)

;; The following runs periodically in the foreground
(use-package org-notifications
  :disabled
  :ensure nil
  :pin melpa
  :config
  (org-notifications-start))

(with-eval-after-load "org-capture"
  (setq org-capture-templates
		(nconc '(("p" "Protocol" entry (file+headline org-default-notes-file "Inbox")
				  "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
				 ("L" "Protocol Link" entry (file+headline org-default-notes-file "Inbox")
				  "* %? [[%:link][%:description]] \nCaptured On: %U")
				 ("t" "Backlog (default)" entry (file+headline my/org-todo-file "Backlog") "* TODO %U %a\n%i\n%?")
				 ("T" "Backlog" entry (file+headline my/org-todo-file "Backlog") "* TODO %U %^{title|chat AI|mail AI}\n%i\n%?")
				 ("b" "bug" entry (file+headline my/org-todo-file "Backlog")
				  "* TODO %(org-buganizer-create-todo-string-from-bug)"
				  :clock-in t :clock-resume t))
			   org-capture-templates)))


;;;; org-roam
(use-package org-roam
  :ensure nil
  :after org
  :custom
  (org-roam-directory org-directory)
  (org-roam-completion-everywhere t)
  :config
  (require 'org-roam-capture)
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
		`(("d" "default" plain "%?" :target
		   (file+head ,(concat my/org-notes-directory "%<%Y%m%d>-${slug}.org") "#+title: ${title}
		#+filetags: %^G
		")
		   :unnarrowed t
		   :kill-buffer)
		  ("r" "recurring" entry "* %u %?" :target
		   (file+head ,(concat my/org-recurring-directory "${slug}.org") "#+title: ${title} ")

		   :unnarrowed t)
		  ("p" "project" plain "%?" :target
		   (file+head ,(concat my/org-projects-directory "${slug}.org") "#+title: ${title}
		#+filetags: %^G
		")
		   :unnarrowed t)))
  (setq org-roam-capture-ref-templates
		`(("r" "ref" plain "%?" :target
		   (file+head ,(concat my/org-notes-directory "%<%Y%m%d>-${slug}.org") "#+title: ${title}
		#+filetags: %^G
		")
		   :unnarrowed t)
		  ("t" "agenda item" entry "* %^{State?|TODO|WORKING|BACKLOGGED} %u ${title}
		%?"
		   :target (file+head ,(concat my/org-agenda-directory "%<%Y%m%d>-${slug}.org") "#+title: ${title}
		#+filetags: %^G
		#+category: %^{Category?|todo|buganizer}
		")
		   :unnarrowed t
		   :kill-buffer)))
  :bind (("C-c n f" . org-roam-node-find)
		 ("C-c n c" . org-roam-capture)
		 (:map org-mode-map
			   ("C-c n i" . org-roam-node-insert)
			   ("C-c n r" . org-roam-ref-add)
			   ("C-c n t" . org-roam-tag-add)
			   ("C-c n b" . org-roam-buffer-toggle))))


(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start t))
;;;; notdeft
;;;; [[https://github.com/hasu/notdeft][notdeft]] is a fast text search engine for my notes, but it requires manual installation.
(setq my-notdeft-package-path (expand-file-name "~/Projects/OpenSource/notdeft"))
(add-to-list 'load-path my-notdeft-package-path)
(add-to-list 'load-path (concat my-notdeft-package-path "/extras"))
(use-package notdeft-autoloads
  :after
  org-roam
  :ensure nil
  :config
  (setq notdeft-directories (list (expand-file-name (concat org-roam-directory))))
  (setq notdeft-xapian-program (expand-file-name (concat my-notdeft-package-path "/xapian/notdeft-xapian"))))
;;; Fun
;; (use-package md4rd)
;;; Local init.el
(defvar my-local-init-file (concat user-emacs-directory "init.local.el") "Local init.el file for per instance configuration.")
(setq custom-file my-local-init-file)

(if (file-exists-p my-local-init-file)
	(load my-local-init-file)
  (write-region "" nil my-local-init-file t))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


;;; Local Variables
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (outline-hide-sublevels 1)
;; End:


;;; init.el ends here
