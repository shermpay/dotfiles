;;; init.el -- Summary
;;; Commentary:
;;; Load my org babel config

;;; Code:
(setq vc-follow-symlinks nil)
;; The following is a workaround as org-babel-load-file has a bug(not following symlinks)
(require 'org)
(let* ((file (expand-file-name "config.org" user-emacs-directory))
	   (tangled-file (concat (file-name-sans-extension file) ".el")))
  ;; Tangle only if the Org file is newer than the Elisp file.
  (unless (org-file-newer-than-p
		   tangled-file
		   (file-attribute-modification-time (file-attributes (file-truename file))))
	(org-babel-tangle-file file tangled-file "emacs-lisp"))
  (load-file tangled-file)
  (message "Loaded %s" tangled-file))

;; Load my org babel config
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (aio vterm evil-string-inflection string-inflection cider xterm-color counsel use-package protobuf-mode projectile moe-theme magit linum-relative helm goto-last-change flycheck-pycheckers exec-path-from-shell evil-tabs evil-leader company)))
 '(safe-local-variable-values (quote ((evil-auto-indent))))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
