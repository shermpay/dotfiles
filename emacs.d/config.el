(require 'cl-lib)

(defvar my-org-dir (concat user-emacs-directory "org"))

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

(setq user-full-name "Jing Hao Sherman Pay"
      user-mail-address "shermanpay1991@gmail.com")

(add-hook 'emacs-startup-hook
          (lambda () (find-file (concat my-org-dir "/todo.org"))))
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

(use-package evil
  :config (evil-mode 1)
  (setq evil-move-cursor-back nil)
  (setq evil-normal-state-cursor '("dim gray" box)
	evil-insert-state-cursor '("dim gray" bar)
	evil-emacs-state-cursor '("green" bar)))

(use-package auto-package-update
  :disabled
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))

(setq browse-url-generic-program "/usr/bin/google-chrome"
      browse-url-browser-function 'browse-url-generic)

(use-package xterm-color)

(use-package eshell
  :after (xterm-color)
  :config
      (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
	(remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

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

(use-package magit)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package protobuf-mode
  :mode "\\.proto")

(use-package bazel-mode
  :mode "BUILD")

(use-package imenu-list
      :bind  ("C-c l"  . #'imenu-list-smart-toggle))

(set-frame-font (find-font (font-spec :name "Hack" :weight 'normal :slant 'normal)) nil t)

(use-package moe-theme)
(moe-dark)

(setq org-hide-leading-stars t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(use-package org-bullets
      :hook (org-mode . (lambda () (org-bullets-mode 1))))

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

(use-package vterm
:pin "melpa")
