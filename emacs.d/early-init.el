;; -*- lexical-binding: t; -*-

(let ((cons-threshold gc-cons-threshold)
      (cons-percentage gc-cons-percentage))

  (setq gc-cons-threshold (* cons-threshold 6))
  (setq gc-cons-percentage (* cons-percentage 6))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold cons-threshold)
              (setq gc-cons-percentage cons-percentage))))

;;; Straight
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; UI
(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
