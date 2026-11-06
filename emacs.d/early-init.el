;; -*- lexical-binding: t; -*-

(let ((cons-threshold gc-cons-threshold)
      (cons-percentage gc-cons-percentage))

  (setq gc-cons-threshold (* cons-threshold 6))
  (setq gc-cons-percentage (* cons-percentage 6))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold cons-threshold)
              (setq gc-cons-percentage cons-percentage))))

;;; Use elpaca
(setq package-enable-at-startup nil)

;;; UI
(setopt default-frame-alist
        '((fullscreen . maximized)))
(setopt initial-frame-alist
        '((fullscreen . fullboth)
          (fullscreen-restore . maximized)))
(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; Local configurations
(defvar my-local-early-init-file
  (concat user-emacs-directory "early-init.local.el")
  "Local early-init.el file for per instance configuration.")

(when (file-exists-p my-local-early-init-file)
  (load my-local-early-init-file))
