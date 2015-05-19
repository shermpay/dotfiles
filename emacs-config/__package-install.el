;;; This is a generated script to install elisp packages
(require (quote package))

(setq package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))

(package-initialize)
(defvar -my-packages (zonokai-theme
yaml-mode
w3m
visual-regexp-steroids
visual-regexp
use-package
undo-tree
unbound
typed-clojure-mode
tablist
sr-speedbar
sql-indent
solarized-theme
smyx-theme
sml-mode
smartparens
smart-mode-line
slime
shm
scion
scala-mode2
s
rich-minority
rainbow-delimiters
racket-mode
queue
quack
python-environment
projectile
powerline
popup
pkg-info
pdf-tools
paredit
noctilux-theme
nginx-mode
multiple-cursors
mode-compile
math-symbol-lists
markdown-mode
magit
linum-relative
let-alist
kibit-mode
key-chord
javadoc-lookup
inf-groovy
helm-projectile
helm
haskell-mode
hackernews
groovy-mode
graphviz-dot-mode
grails-mode
gradle-mode
goto-chg
gotest
go-mode
go-eldoc
gnuplot-mode
gnuplot
git-timemachine
git-rebase-mode
git-commit-mode
geiser
fringe-helper
flycheck-pos-tip
flycheck-clojure
flycheck
faceup
f
expand-region
exec-path-from-shell
evil-tabs
evil-surround
evil-paredit
evil-org
evil-leader
evil
epl
epc
emacs-eclim
elscreen
elein
ecb
dropdown-list
diminish
deferred
dash
dart-mode
ctable
concurrent
company-go
company
color-theme
clojurescript-mode
clojure-mode-extra-font-locking
clojure-mode
clojure-cheatsheet
cider
c-eldoc
bind-key
auto-indent-mode
async
ant
align-cljlet
ace-jump-mode
))

(dolist (pkg -my-packages) (when (not (package-installed-p pkg)) (with-demoted-errors (package-install pkg))))