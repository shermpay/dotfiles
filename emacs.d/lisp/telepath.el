;;; telepath.el --- Functions for managing remote processes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sherman Pay

;; Author: Sherman Pay <shermanpay1991@gmail.com>
;; Keywords: processes, unix

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

(require 'cl-lib)

(require 'my-vterm)

(defvar telepath-exec-binary-path (concat (file-name-as-directory (getenv "HOME")) "bin/telepath_exec")
  "Path to a binary that executes commands on a remote host via SSH.

The binary is expected to take argument in the following format [SSH ARGS...] -- CMD [CMD ARGS...]")
(defvar telepath-ssh-binary-path "/usr/local/bin/ssh")
(defvar telepath-process-format "*telepath-process[%s<ssh:%s>$ %s]*")

(defun telepath--process-name (host ssh-args command)
  "Returns a name formatted with TELEPATH-PROCESS-FORMAT."
  (format telepath-process-format host (or ssh-args "()") command))

(cl-defun telepath-start-process (host &key (ssh-args (list)) command name buf)
  "Start a process on HOST."
  (cl-check-type host string)
  (dolist (a ssh-args)
    (cl-check-type a string))
  (cl-check-type command cons)
  (dolist (c command)
    (cl-check-type c string))
  (let* ((name (if name name (telepath--process-name host ssh-args command)))
         (buf (if buf buf (get-buffer-create name))))
    (apply #'start-process
           name
           buf
           telepath-ssh-binary-path
           host
           (string-join ssh-args " ")
           command)))


(defvar telepath--local-port)


(defun telepath-tmux-list-sessions (host)
  "List tmux sessions on HOST."
  (with-temp-buffer
    (call-process telepath--ssh-binary-path
                  nil                   ; INFILE
                  t                     ; DESTINATION=current-buffer
                  t                     ; DISPLAY
                  host "tmux" "list-sessions" "-F" "'#{session_name}'")
    (split-string (buffer-string) "[ \f\t\n\r\v]+" t)))


(defun telepath-tmux-list-windows (host session)
  "List windows in SESSION on HOST."
  (with-temp-buffer
    (call-process telepath--ssh-binary-path
                  nil                   ; INFILE
                  t                     ; DESTINATION=current-buffer
                  t                     ; DISPLAY
                  host
                  "tmux" "list-windows"
                  "-t" session
                  "-F" "'#{session_name};#{window_name};#{pane_pid}'")
    (split-string (buffer-string) "[\n\r]+" t)))

(cl-defun telepath-start-tmux-process (host &key args command tmux-session tmux-window)
  "Start a process on HOST.

ARGS is a list of additional SSH arguments to pass to SSH.

COMMAND is a list of strings representing the shell command to start the remote process.

TMUX-SESSION is the tmux session to run the process in.

TMUX-WINDOW is the tmux window to run the process in."
  (cl-check-type host string)
  (dolist (a args)
    (cl-check-type a string))
  (cl-check-type command cons)
  (dolist (c command)
    (cl-check-type c string))
  (unless (null tmux-session) (cl-check-type tmux-session string))
  (unless (null tmux-window) (cl-check-type tmux-window string))
  (let* ((command-str (string-join command " "))
         (command-short-str (string-join (cons (file-name-nondirectory
                                                (first command))
                                               (rest command))
                                         " "))
         (name (telepath--process-name host args command-short-str))
         (tmux-wrapped (list "tmux" "new-session"
                             "-s" tmux-session
                             "-t" tmux-session
                             "-A"
                             "\\;"
                             "new-window"
                             "-n" (format "'%s'" command-short-str)
                             command-str))
         (ssh-command (format "%s -t %s %s \"%s\""
                              telepath-ssh-binary-path
                              (string-join args " ")
                              host
                              (string-join tmux-wrapped " "))))
    (message "telepath.el: running command='%s' in vterm" ssh-command)
    (my-vterm-start-process ssh-command
                            :buffer name)))


(when nil
  (telepath-start-tmux-process "ramuh" :command "ls" :tmux-session "telepath" :tmux-window ""))

(provide 'telepath)
;;; telepath.el ends here
