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

(require 'ansi-color)

(setq debug-on-error t)

(let ((tmp (concat user-emacs-directory "tmp")))
 (setq backup-directory-alist
       `((".*" . ,tmp)))
 (setq auto-save-file-name-transforms
       `((".*" ,tmp t))))

(global-linum-mode t)
(column-number-mode t)

(menu-bar-mode -1) 			
(menu-bar-no-scroll-bar)
(tool-bar-mode -1)

(tooltip-mode -1)
(setq echo-keystrokes 0.01)

(setq server-port 1337)
(setq server-use-tcp t)
(setq server-host (system-name))
(server-start) ; Start the emacs server

(require 'package)
(add-to-list 'package-archives
      '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package evil
  :config (evil-mode 1)
  (setq evil-move-cursor-back nil)
  (setq evil-normal-state-cursor '("dim gray" box)
	evil-insert-state-cursor '("dim gray" bar)
	evil-emacs-state-cursor '("blue" bar)))

(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))

(setq browse-url-generic-program "/usr/bin/google-chrome"
      browse-url-browser-function 'browse-url-generic)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package helm
  :config (helm-mode 1)
  (setq helm-M-x-fuzzy-match t
	helm-split-window-inside-p t)
  :bind ("M-x" . helm-M-x))

(use-package projectile
  :config (projectile-global-mode 1)
  :bind-keymap
  ("C-c C-p" . projectile-command-map)
  ("C-c p" . projectile-command-map))

(use-package undo-tree
  :config (global-undo-tree-mode 1))

(use-package company
  :config (global-company-mode))

(use-package magit)

(use-package flycheck
  :config (global-flycheck-mode))

(set-frame-font (find-font (font-spec :name "Hack" :weight 'normal :slant 'normal)))

(use-package moe-theme)
(moe-dark)

(use-package flycheck-pycheckers
  :config (with-eval-after-load 'flycheck
	    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))
