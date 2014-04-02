;;; Script to install packages

(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
	("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar editing '(ace-jump-mode
		  expand-region
		  undo-tree
		  multiple-cursors
		  visual-regexp
		  visual-regexp-steroids))

(defvar clojure '(ac-cider-compliment
		  ac-nrepl
		  align-cljlet
		  clojure-mode
		  clojure-test-mode
		  cider
		  clj-refactor
		  clojure-cheatsheet
		  clojure-mode))

(defvar prog '(ac-octave
	       markdown-mode
	       groovy-mode
	       auto-complete
	       slime
	       jedi
	       magit
	       rainbow-delimiters
	       paredit
	       sml-mode
	       projectile))

(defvar misc '(dropdown-list
	       flx
	       flx-ido
	       elscreen
	       ido-ubiquitous
	       smex
	       key-chord))


(defvar evil '(evil))


(defvar all-pkgs (list editing clojure prog misc evil))
;;; Installing editing packages

(dolist (all-pkgs pkgs)
 (dolist (pkg pkgs)
   (when (not (package-installed-p pkg))
     (with-demoted-errors (package-install pkg)))))
