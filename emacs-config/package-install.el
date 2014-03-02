;;; Script to install packages

(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
	("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(
		      ace-jump-mode
		      ac-cider-compliment
		      ac-nrepl
		      ac-octave
		      align-cljlet
		      auto-complete
                      clojure-mode
                      clojure-test-mode
                      cider
		      clj-refactor
		      clojure-cheatsheet
		      clojure-mode
		      clojure-snippets
		      dash
		      dropdown-list
		      elscreen
		      epc
		      expand-region
		      evil
		      f
		      flx
		      flx-ido
		      groovy-mode
		      hackernews
		      ido-ubiquitous
		      jedi
		      key-chord
		      magit
		      markdown-mode
		      multiple-cursors
		      mic-paren
		      noctilux-theme
		      paredit
		      popup
		      powerline
		      projectile
		      rainbow-delimiters
		      s
		      slime
		      smex 
		      sml-mode
		      unbound
		      undo-tree
		      visual-regexp
		      visual-regexp-steroids
		      yasnippet))

(dolist (pkg my-packages)
  (when (not (package-installed-p pkg))
    (ignore-errors (package-install pkg))))
