;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'google-c-style)

(add-hook 'c-mode-hook (lambda ()
                         (c-set-style "linux" nil)))

(add-hook 'c++-mode-hook (lambda ()
                           (c-set-style "google" nil)))

(require 'clang-format)
(add-hook 'before-save-hook (lambda ()
                              (when (eq major-mode 'c++-mode) (clang-format-buffer))))

(defvar *my-llvm-config* "/bin/llvm-config")
(defvar *my-llvm-config-args* " --cxxflags")

(provide 'c)
;; <<<<<<<<<<<<<<<<<<<< END C >>>>>>>>>>>>>>>>>>>>
