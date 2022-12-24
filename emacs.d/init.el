;;; init.el -- Summary
;;; Commentary:
;;; Load my org babel config

;;; Code:
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

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
