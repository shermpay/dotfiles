;;;;;;;;;;;;;;;
;; Jedi mode ;;
;;;;;;;;;;;;;;;
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;;;;
;;     Python Mode ;;
;;;;;;;;;;;;;;;;;;;;;
(setq python-indent-offset 4)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda () (electric-indent-mode 0)))

(setq python-shell-interpreter "ipython3"
       python-shell-interpreter-args ""
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code
       "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code
       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n" )
(provide 'py)
