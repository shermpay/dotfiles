;;; Script to generate package installer
(defun generate-package-initializers ()
  (print '(require 'package))
  (print '(setq package-archives
                '(("gnu"         . "http://elpa.gnu.org/packages/")
                  ("marmalade"   . "http://marmalade-repo.org/packages/")
                  ("melpa"       . "http://melpa.milkbox.net/packages/"))))
  (print '(package-initialize)))

(defun generate-my-packages-list ()
  (princ "(defvar -my-packages (")
  (dolist (pkg package-alist)
    (destructuring-bind (pkg-name . pkg-info) pkg
      (princ (format "%S\n" pkg-name))))
  (princ "))\n")
  t)

(defun generate-package-installation-script ()
  (with-temp-file "-package-install.el"
   (with-output-to-temp-buffer (current-buffer)
     (princ ";;; This is a generated script to install elisp packages")
     (generate-package-initializers)
     (print '(make-local-variable -my-packages))
     (generate-my-packages-list)
     (print
      '(dolist (pkg -my-packages)
         (when (not (package-installed-p pkg))
           (with-demoted-errors (package-install pkg))))))))

(generate-package-installation-script)
