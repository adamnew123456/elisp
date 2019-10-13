;;; -*- lexical-binding: t -*-
;;;
;;; Emacs functions for clk2. Does not currently support history rewriting, although that will be
;;; coming soon

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
  "Calls a JSON-RPC method on the clk2 server"
  (let ((json-object-type 'hash-table)
        (request (make-hash-table))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json-rpc"))))

    (puthash "jsonrpc" "2.0" request)
    (puthash "id" 1 request)
    (puthash "method" method request)
    (puthash "params" params request)
    (let ((url-request-data
           (json-encode-hash-table request)))

      (with-current-buffer (url-retrieve-synchronously clk2-url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (clk2//parse-rpc-response (json-read))))))

(defun clk2//format-seconds (seconds)
  "Converts a duration in seconds to a human-readable format"
  (let* ((hours (/ seconds (* 60 60)))
         (remainder (% seconds (* 60 60)))
         (minutes (/ remainder 60))
         (seconds (% remainder 60)))
    (format "%d:%02d:%02d" hours minutes seconds)))

(defun clk2/get-tasks ()
  "Gets a list of task IDs known to the server"
  (mapcar
   (lambda (task) (gethash "id" task))
   (clk2//call-rpc-method "list")))

(defun clk2//choose-task (allow-new-task callback)
  "Asks the user to pick a task and runs the callback with it if they do"
  (let* ((tasks (clk2/get-tasks))
         (tasks-source (helm-build-sync-source "Tasks" :candidates tasks))
         (sources (if allow-new-task
                      (list tasks-source clk-source-new-task)
                    tasks-source))
         (selected-task (helm :sources sources)))

    (if selected-task
        (funcall callback selected-task))))

(defun clk2/in ()
  "Clocks in to a task"
  (interactive)
  (clk2//choose-task t
                     (lambda (task)
                       (clk2//call-rpc-method "start" task))))

(defun clk2/out ()
  "Clocks out of a task"
  (interactive)
  (clk2//choose-task nil
                     (lambda (task)
                       (clk2//call-rpc-method "stop" task))))

(defun clk2/finish ()
  "Finishes a task and shows the completion time"
  (interactive)
  (clk2//choose-task nil
                     (lambda (task)
                       (let ((elapsed (clk2//call-rpc-method "finish" task)))
                         (message "clk2: Task %s took %s"
                                  task
                                  (clk2//format-seconds elapsed))))))

(defun clk2/list ()
  "Displays a list of all tasks"
  (interactive)
  (let ((tasks (clk2//call-rpc-method "list")))
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
      (pop-to-buffer "*clk2 summary*"))))

(defun clk2/history ()
  "Displays a log for a particular task"
  (interactive)
  (clk2//choose-task nil
                     (lambda (task)
                       (let ((buffer-name (format "*clk2: %s*" task))
                             (history (clk2//call-rpc-method "history" task)))

                         (with-output-to-temp-buffer buffer-name
                           (mapc
                            (lambda (event)
                              (princ (gethash "timestamp" event))
                              (princ "; ")
                              (princ (gethash "event" event))
                              (princ "; ")
                              (princ (clk2//format-seconds (gethash "cumulative_sec" event)))
                              (princ "\n"))
                            history)
                           (pop-to-buffer buffer-name))))))
