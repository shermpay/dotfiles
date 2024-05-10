;;; my-vterm.el --- My improvements on top of vterm  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sherman Pay

;; Author: Sherman Pay <shermanpay1991@gmail.com>
;; Keywords: terminals, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'vterm)

(defun my-vterm--list-buffers ()
  "List all buffers where the major-mode is vterm-mode."
  (seq-filter (lambda (buffer) (with-current-buffer buffer (equal major-mode 'vterm-mode)))
              (buffer-list)))

(defun my-vterm--kill-process (process event)
  "A process sentinel; kill PROCESS buffer if it is live.

EVENT is a string describing the change."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun my-vterm-run (command &optional buffer)
  "Execute string COMMAND in BUFFER creating a new buffer if it does not exist.

Interactively, prompt for COMMAND with the current buffer's file
name supplied.  When called from Dired, supply the name of the
file at point.

BUFFER is a string buffer name.

Like `async-shell-command`, but run in a vterm for full terminal features.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (let* ((command (read-shell-command "command: "))
          (vterm-buffers (mapcar #'buffer-name (my-vterm--list-buffers))))
     (list command
           (completing-read "buffer: " (cons (concat "*" command "*") vterm-buffers)))))
  (let* ((vterm-buffers (mapcar #'buffer-name (my-vterm--list-buffers)))
         (new-vterm (null (member buffer vterm-buffers)))
         (dir default-directory))
    (with-current-buffer (if new-vterm (vterm buffer) buffer)
      ;; This is needed so vterm does not rename the buffer
      (setq-local vterm-buffer-name-string nil)
      (when new-vterm (set-process-sentinel vterm--process #'my-vterm--kill-process))
      (vterm-send-string (format "cd %s" dir))
      (vterm-send-return)
      (vterm-send-string command)
      (vterm-send-return)
      (vterm--set-title buffer)
      (rename-buffer buffer)
      (pop-to-buffer (current-buffer))
      (current-buffer))))

(defun my-vterm-start-process (command &key buffer)
  "Execute string COMMAND in BUFFER creating a new buffer if it does not exist.

Runs the COMMAND as the startup program.

BUFFER is a string buffer name, and defaults to *vterm-process*.

Returns the vterm buffer."

  (let ((original-shell vterm-shell)
        (buffer (if (stringp buffer) buffer "*vterm-process*")))
    (setq vterm-shell command)
    (with-current-buffer (unwind-protect
                             (vterm buffer)
                           (setq vterm-shell original-shell))
      (set-process-sentinel vterm--process #'my-vterm--kill-process)
      (rename-buffer buffer)
      (current-buffer))))


(provide 'my-vterm)
;;; my-vterm.el ends here
