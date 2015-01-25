;; ^^^^^^^^^^^^^^^ Java MODE ^^^^^^^^^^^^^^^
(add-hook 'java-mode-hook 'java-defer-loading)
(add-to-list 'load-path "~/.emacs.d/plugins/auto-java-complete/")
;; (require 'ajc-java-complete-config)
;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)
;; (add-hook 'find-file-hook 'auto-4-jsp-find-file-hook)
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
;; (require 'malabar-mode)
;; (setq malabar-groovy-lib-dir "/path/to/malabar/lib")
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(add-hook 'java-mode-hook
      '(lambda ()
         "Treat Java 1.5 @-style annotations as comments."
         (setq c-comment-start-regexp 
           "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
         (modify-syntax-entry ?@ "< b" 
                  java-mode-syntax-table)))
;; <<<<<<<<<<<<<<< END JDE >>>>>>>>>>>>>>>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Groovy-mode           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set path to groovy binary
(require 'groovy-mode)
(setq groovy-home "/home/shermpay/.gvm/groovy/current")
(setq groovy-program-name "groovysh")
(provide 'java)
