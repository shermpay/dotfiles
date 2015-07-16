;;; package -- Summary
;;; Commentary:
;;;;;;;;;;;;;;;;;;
;; Haskell Mode ;;
;;;;;;;;;;;;;;;;;;

;;; Code:
(defvar haskell-key-bindings)
(setq haskell-key-bindings
          `((,(kbd "C-c C-z") . run-haskell)
            (,(kbd "C-c C-d") . hoogle)))

(defun haskell-config ()
  "Configure haskell."
  (progn
    (set (make-local-variable 'electric-indent-mode) nil)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)
    (dolist (key-bindings haskell-key-bindings)
      (destructuring-bind (key . f) key-bindings
        (define-key haskell-mode-map key f)))))

(add-hook 'haskell-mode-hook 'haskell-config)

(message "Loaded Haskell Config")
(provide 'hs)

;;; hs.el ends here
