;; ^^^^^^^^^^^^^^^ Java MODE ^^^^^^^^^^^^^^^
(add-hook 'java-mode-hook 'java-defer-loading)
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

(defun initialize-eclim ()
  (require 'eclim)
  (global-eclim-mode)
  (setq eclim-eclipse-dirs '("~/Software/eclipse"))
  (setq eclim-executable "~/Software/eclipse/eclim")
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (require 'eclimd)
  (setq eclimd-default-workspace "~/Software/eclim")
  (start-eclimd eclimd-default-workspace)
  (setq company-eclim-auto-save t)
  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)
  (company-eclim))

(add-hook 'java-mode-hook 'initialize-eclim)

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
