;;; -*- lexical-binding: t -*-
;; Copyright 2019, Chris Marchetti

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; Emacs functions for clk2, github.com/adamnew123456/clk2

(require 'elfuture)
(require 'json)
(require 'helm)
(require 'seq)
(require 'url)

(defvar clk2-url "http://localhost:6996/"
  "The URL where your local clk2 server accepts connections")

(defvar clk-source-new-task (helm-build-dummy-source "New Task"))

(defun clk2//parse-rpc-response (response)
  "Parses a JSON-RPC response into a result or raises an error"
  (let ((response-error (gethash "error" response '()))
        (response-data (gethash "result" response '())))
    (cond
     ((and response-error (gethash "data" response-error))
      (error "clk2 received server error: %s"
             (gethash "Message" (gethash "data" response-error))))

     (response-error
      (error "clk2 received server error: %s"
             (gethash "message" response-error)))

     (t response-data))))

(defun clk2//call-rpc-method (method &rest params)
  "(async) Calls a JSON-RPC method on the clk2 server"
  (let ((request (make-hash-table))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json-rpc"))))

    (puthash "jsonrpc" "2.0" request)
    (puthash "id" 1 request)
    (puthash "method" method request)
    (puthash "params" params request)
    (let ((url-request-data
           (json-encode-hash-table request)))

      (elfuture-attach
       (elfuture-retrieve-url clk2-url)
       (lambda (buffer)
         (with-current-buffer buffer
           (goto-char (point-min))
           (re-search-forward "^$")
           (let ((json-object-type 'hash-table))
             (clk2//parse-rpc-response (json-read)))))))))

(defun clk2//format-seconds (seconds)
  "Converts a duration in SECONDS to a human-readable format"
  (let* ((hours (/ seconds (* 60 60)))
         (remainder (% seconds (* 60 60)))
         (minutes (/ remainder 60))
         (seconds (% remainder 60)))
    (format "%d:%02d:%02d" hours minutes seconds)))

(defun clk2/get-tasks ()
  "(async) Gets a list of task IDs known to the server"
  (elfuture-attach
   (clk2//call-rpc-method "list")
   (lambda (tasks)
     (mapcar (lambda (task) (gethash "id" task))
             tasks))))

(defun clk2//choose-task (allow-new-task)
  "(async) Asks the user to pick a task. New tasks are allowed if ALLOW-NEW-TASK is true."
  (elfuture-attach
   (clk2/get-tasks)
   (lambda (tasks)
     (let* ((tasks-source (helm-build-sync-source "Tasks" :candidates tasks))
            (sources (if allow-new-task
                         (list tasks-source clk-source-new-task)
                       tasks-source))
            (selected-task (helm :sources sources)))

       selected-task))))

(defun clk2/in ()
  "Clocks in to a task"
  (interactive)
  (elfuture-attach
   (clk2//choose-task t)
   (lambda (task)
    (if task (clk2//call-rpc-method "start" task)))))

(defun clk2/out ()
  "Clocks out of a task"
  (interactive)
  (elfuture-attach
   (clk2//choose-task nil)
   (lambda (task)
    (if task (clk2//call-rpc-method "stop" task)))))

(defun clk2/finish ()
  "Finishes a task and shows the completion time"
  (interactive)
  (let ((the-task nil))
    (elfuture-attach
     (clk2//choose-task nil)
     (lambda (task)
       (if (null task) (return nil))
       (setq the-task task)
       (clk2//call-rpc-method "finish" task))
     (lambda (elapsed)
       (message "clk2: Task %s took %s"
                the-task
                (clk2//format-seconds elapsed))))))

(defun clk2/list ()
  "Displays a list of all tasks"
  (interactive)
  (elfuture-attach
   (clk2//call-rpc-method "list")
   (lambda (tasks)
    (with-output-to-temp-buffer "*clk2 summary*"
     (mapc
      (lambda (task)
       (princ (gethash "id" task))
       (princ "; ")
       (princ (gethash "status" task))
       (princ "; ")
       (princ (clk2//format-seconds (gethash "elapsed_sec" task)))
       (princ "\n"))
      tasks)
     (pop-to-buffer "*clk2 summary*")))))

(defun clk2/history ()
  "Displays a log for a particular task"
  (interactive)
  (elfuture-attach
   (clk2//choose-task nil)
   (lambda (task)
    (clk2//call-rpc-method "history" task))
   (lambda (history)
     (with-output-to-temp-buffer "*clk2 history*"
       (mapc
        (lambda (event)
          (princ (gethash "timestamp" event))
          (princ "; ")
          (princ (gethash "event" event))
          (princ "; ")
          (princ (clk2//format-seconds (gethash "cumulative_sec" event)))
          (princ "\n"))
        history)
       (pop-to-buffer "*clk2 history*")))))
