;; Go configuration
(setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports"))
(add-to-list 'load-path (concat (getenv "GOROOT") "/misc/emacs/"))

(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)
(setq default-tab-width 4)

(require 'company-go)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)
                                        ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(add-hook 'go-mode-hook 'go-eldoc-setup)

(provide 'go)
