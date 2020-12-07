(require 'cl-lib)

(defvar my-gdrive-dir (getenv "MY_DRIVE"))
(defun my-gdrive-or-emacs-dir ()
      (if my-gdrive-dir
	      my-gdrive-dir
	user-emacs-directory))

(let ((default-directory user-emacs-directory))
      (normal-top-level-add-subdirs-to-load-path))

(setq user-full-name "Jing Hao Sherman Pay"
      user-mail-address "shermanpay1991@gmail.com")

(add-hook 'emacs-startup-hook
		      (lambda () (find-file (concat (my-gdrive-or-emacs-dir) "/org/todo.org"))))
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(define-key global-map (kbd "M-o") 'other-window)

(global-subword-mode t)

(show-paren-mode 1)
(setq show-paren-style 'expression)

(setq next-line-add-newlines nil)

(save-place-mode 1)

(setq-default tab-width 4)

(require 'ansi-color)

(setq debug-on-error t)

(let ((tmp (concat user-emacs-directory "tmp")))
 (setq backup-directory-alist
       `((".*" . ,tmp)))
 (setq auto-save-file-name-transforms
       `((".*" ,tmp t))))

(global-linum-mode -1)
(column-number-mode t)

(menu-bar-mode -1) 			
(menu-bar-no-scroll-bar)
(tool-bar-mode -1)

(tooltip-mode -1)
(setq echo-keystrokes 0.01)

(define-key global-map (kbd "C-c i") 'imenu)

(define-key global-map (kbd "C-x C-b") 'ibuffer)

(server-start)

(desktop-save-mode 1)
(add-to-list 'desktop-path "~/.emacs.d/desktops")

(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))

(setq browse-url-generic-program "/usr/bin/google-chrome"
      browse-url-browser-function 'browse-url-generic)

(require 'package)
(add-to-list 'package-archives
      '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
      '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

(use-package auto-package-update
  :disabled
  :config
      (setq auto-package-update-interval 90)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
      (setq auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

(use-package evil
  :config (evil-mode 1)
  (setq evil-move-cursor-back nil)
  (setq evil-normal-state-cursor '("dim gray" box)
	evil-insert-state-cursor '("dim gray" bar)
	evil-emacs-state-cursor '("green" bar)))

;; (use-package xterm-color)

;; (use-package eshell
;;   :after (xterm-color)
;;   :config
;;   (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
;;   (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;   (setq eshell-output-filter-functions
;; 	(remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package counsel
  :diminish (ivy-mode "")
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package projectile
  :diminish (projectile-mode . "")
  :config (projectile-global-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package undo-tree
  :diminish (undo-tree-mode . "")
  :config (global-undo-tree-mode 1))

(use-package company
  :diminish (company-mode . "")
  :config (global-company-mode))

(use-package magit
      :pin melpa)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package protobuf-mode
  :mode "\\.proto")

(use-package bazel-mode
  :mode "BUILD")

(use-package imenu-list
      :bind  ("C-c l"  . #'imenu-list-smart-toggle))

(use-package eglot
      :pin melpa-stable
      :config (add-to-list 'eglot-stay-out-of 'imenu))

(set-frame-font (find-font (font-spec :name "Hack" :weight 'normal :slant 'normal)) nil t)

(use-package moe-theme
      :config (moe-dark))

(setq org-hide-leading-stars t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-directory (concat (my-gdrive-or-emacs-dir) "/org/"))
(setq org-default-notes-file (concat org-directory "notes.org"))
(defconst my-org-todo-file (concat org-directory "todo.org"))

(defun my-org-mode-config-minor-modes ()
      ;; disable
      (flycheck-mode -1)
      ;; enable
      (visual-line-mode))
(add-hook 'org-mode-hook #'my-org-mode-config-minor-modes)

(nconc org-modules
		'(
		      org-tempo
		      org-capture
		      protocol
		      ;; org-habit
		      ;; org-id
		      ;; org-brain
		      ))
(eval-after-load 'org
	'(org-load-modules-maybe t))
(use-package org-bullets
      :hook (org-mode . (lambda () (org-bullets-mode 1))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp t)
       (dot . t)))

(setq org-capture-templates '(
	("p" "Protocol" entry (file+headline org-default-notes-file "Inbox")
		"* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file+headline org-default-notes-file "Inbox")
		"* %? [[%:link][%:description]] \nCaptured On: %U")
	("t" "Action Item" entry (file+headline my-org-todo-file "Backlog") "* TODO %U %a\n%i\n%?")
	("T" "Action Item" entry (file+headline my-org-todo-file "Backlog") "* TODO %U %^{title|chat AI|mail AI}\n%i\n%?")
	("b" "bug" entry (file "~/bugs.org")
	 "* TODO %(org-buganizer-create-todo-string-from-bug)"
	 :clock-in t :clock-resume t)))

(defun my-save-org-archive-file ()
      "Save org-archive file."
      (interactive)
      (if (equal (file-name-extension buffer-file-name) "org")
	      (with-current-buffer (concat (buffer-name) "_archive")
		(save-buffer))
	nil))

;; Finally, the newly-defined function can advise the archive function. So,
;; after a subtree in org is archived, the archive file will be automatically saved.
(advice-add 'org-archive-subtree :after #'my-save-org-archive-file)


(defun my-org-table-to-dot (nodes edges &optional attr subgraph)
      "Generate a graph in dot format given NODES and EDGES."
      (concat
       "digraph {\n"
       (mapconcat 'identity attr "\n")
       "\n"
       (mapconcat
	(lambda (x)
	      (format "%s [label=\"%s\" shape=%s style=\"filled\" fillcolor=\"%s\"];"
			      (car x)
			      (nth 1 x)
			      (if (string= "" (nth 2 x)) "box" (nth 2 x))
			      (if (string= "" (nth 3 x)) "none" (nth 3 x))
			      )) nodes "\n")
       "\n"
       (mapconcat
	(lambda (x)
	      (format "%s -> %s [taillabel=\"%s\"];"
			      (car x) (nth 1 x) (nth 2 x))) edges "\n")
       "}\n"
       subgraph
       "\n"))

(use-package google-c-style
  :pin melpa)

(use-package flycheck-pycheckers
  :config (with-eval-after-load 'flycheck
	    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package company-jedi
  :requires company
  :config
  (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi))

(defun add-hook-gofmt-before-save ()
      (add-hook 'before-save-hook 'gofmt-before-save nil t))
(add-hook 'go-mode-hook 'add-hook-gofmt-before-save)

(use-package company-go
      :config
      (setq company-tooltip-limit 20)                       ; bigger popup window
      (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
      (setq company-echo-delay 0)                           ; remove annoying blinking
      (setq company-begin-commands '(self-insert-command))) ; start autocompletion only after typing

(add-hook 'go-mode-hook (lambda ()
						      (set (make-local-variable 'company-backends) '(company-go))
						      (company-mode)))

(use-package paredit
      :commands (enable-paredit-mode)
      :init
      (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(use-package vterm
  :pin "melpa"
  :hook (vterm-mode . (lambda () (goto-address-mode 1))))
