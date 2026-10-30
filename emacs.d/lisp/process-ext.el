;;; process-ext.el --- Process extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Sherman Pay

;; Author: Sherman Pay <shermanpay1991@gmail.com>
;; Keywords: lisp, convenience

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

(require 'cl-lib)

;;; Code:
(defun process-ext--auto-restart-sentinel (ps event)
  "Process sentinel that automatically restarts the process.

PS is the process for which the event occurred.
EVENT is a string describing the event.

See `(elisp) Sentinels'."
  ;; TODO: Insert termination messages in the process buffer?
  (if (not (process-get ps :process-ext))
      (message "ps: %s is not a process-ext process; plist: %s" ps (process-plist ps))
    (let ((deadline (process-get ps :deadline)))
      (if (time-less-p deadline (current-time))
          (message "ps: %s exceeded deadline: lisp-format=%s, human-readable=%s"
                   ps deadline (format-time-string "%Y-%m-%d %T" deadline))
        (cl-case (process-status ps)
          ;; TODO: When to remove from list.
          ;; TODO: Handle fail to start
          ((exit) (let ((new-ps (make-process :name (process-name ps)
                                              :buffer (process-buffer ps)
                                              :command (process-command ps)
                                              :noquery nil
                                              :sentinel #'process-ext--sentinel)))
                    (set-process-plist new-ps (process-plist ps))
                    ps))
          (otherwise (message "process-ext-process: %s encountered event: %s"  ps event)))))))


(cl-defun process-ext-start (&key name buffer command (duration 0))
  "Start a process that stays alive for DURATION.

NAME is the name for the process.  It is modified if necessary to make it
unique.

BUFFER is the buffer (or `buffer-name') to associate with the process.

COMMAND is a list starting with the program file name, followed by strings to
give to the program as arguments.

DURATION is the number of seconds to keep the process alive. The process isn't
kill after duration, but if it exits after seconds, do not restart the process."
  (let* ((current-time-list t)
         (ps (make-process :name name
                           :buffer buffer
                           :command command
                           :noquery nil
                           :sentinel #'process-ext--auto-restart-sentinel)))
    (set-process-plist ps `(:process-ext t :deadline ,(time-add nil duration)))
    ps))


(provide 'process-ext)

;;; recur.el ends here
