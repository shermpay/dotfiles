;; Go configuration
(setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports"))
(add-to-list 'load-path (concat (getenv "GOROOT") "/misc/emacs/"))

(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)
(setq default-tab-width 4)

(require 'go-autocomplete)
(require 'auto-complete-config)

(add-hook 'go-mode-hook 'go-eldoc-setup)

(provide 'go)
