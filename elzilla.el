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

(require 'elfuture)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'seq)
(require 'url)

(defvar elzilla-url-base "http://me.com/bugzilla"
  "The base URL of your Bugzilla installation, without a trailing slash

If you were to visit this URL in your web browser, you should see the Bugzilla home page")

(defvar elzilla-api-key "abcdefGHIJKL1234567890"
  "The API key to use for requests to the Bugzilla REST API")

(defvar elzilla-self "me@me.com"
  "The email address of your Bugzilla account.

This is used to find the bugs which are assigned to or created by you using
elzilla/get-my-assigned-bugs and elzilla/get-my-created-bugs")

(defvar elzilla-attachments-dir "/home/me/Downloads"
  "The directory where attachments should be downloaded

Note that this value is used as a prefix, and elzilla will create a subdirectory
for each bug as you download attachments for it. For example, if you download an
attachment called file.pdf for bug 12345 then the attachment will be located at:

  ELZILLA-ATTACHMENTS-DIR/12345/file.pdf")

(defun elzilla//xj (path value)
  "Traverses multiple levels of a JSON structure at once"
  (seq-reduce (lambda (obj key)
                (if (numberp key)
                    (elt obj key)
                  (gethash key obj)))
              path
              value))

(defun elzilla//fetch-json-from-url (url path)
  "(async) Downloads the URL and parses the contents as JSON, and retrieves the PATH."
  (elfuture-attach
   (elfuture-retrieve-url url)
   (lambda (buffer)
     (with-current-buffer buffer
       (goto-char (point-min))
       (re-search-forward "^$")
       (let ((json-object-type 'hash-table))
         (json-read))))
   (lambda (value)
     (elzilla//xj path value))))

(defun elzilla//rest-url (path formatters params)
  "Builds a URL from the Bugzilla REST API base"
  (let* ((full-params (cons (cons "api_key" elzilla-api-key) params))
         (params-string
          (string-join (mapcar (lambda (param)
                                 (concat (url-hexify-string (car param))
                                         "="
                                         (url-hexify-string (cdr param))))
                               full-params)
                       "&")))
    (concat elzilla-url-base
            "/rest.cgi/"
            (apply #'format (cons path formatters))
            "?"
            params-string)))

(defun elzilla//princf (fmt &rest args)
  "Prints to the current output using format"
  (princ (apply #'format (cons fmt args))))

(defun elzilla//browser-link (bug)
  "Returns a link which can be browsed to using the system web browser"
  (format "%s/show_bug.cgi?id=%s" elzilla-url-base bug))

(defun elzilla//print-bug-long (bug)
  "Prints a detailed listing of the bug's information"
  (let ((id (elzilla//xj '("id") bug))
        (summary (elzilla//xj '("summary") bug))
        (status (elzilla//xj '("status") bug))
        (resolution (elzilla//xj '("resolution") bug))
        (updated (elzilla//xj '("last_change_time") bug))
        (assigned-name (elzilla//xj '("assigned_to_detail" "real_name") bug))
        (assigned-email (elzilla//xj '("assigned_to") bug))
        (creator-name (elzilla//xj '("creator_detail" "real_name") bug))
        (creator-email (elzilla//xj '("creator") bug))
        (created (elzilla//xj '("creation_time") bug))
        (product (elzilla//xj '("product") bug))
        (component (elzilla//xj '("component") bug))
        (priority (elzilla//xj '("priority") bug))
        (severity (elzilla//xj '("severity") bug)))
    (elzilla//princf "* Bug %d Info\n" id)
    (elzilla//princf "- [[elzilla:%d][Reload]]\n" id)
    (elzilla//princf "- [[%s][Open in Browser]]\n" (elzilla//browser-link id))
    (elzilla//princf "- Summary: %s\n" summary)
    (elzilla//princf "- Status: %s\n" status)
    (elzilla//princf "- Resolution: %s\n" resolution)
    (elzilla//princf "- Last updated on: %s\n\n" updated)

    (elzilla//princf "- Assigned to: %s (%s)\n" assigned-name assigned-email)
    (elzilla//princf "- Created by: %s (%s)\n" creator-name creator-email)
    (elzilla//princf "- Created on: %s\n\n" created)

    (elzilla//princf "- Product: %s\n" product)
    (elzilla//princf "- Component: %s\n\n" component)

    (elzilla//princf "- Priority: %s\n" priority)
    (elzilla//princf "- Severity: %s\n" severity)))

(defun elzilla//print-attachment (attachment)
  "Prints metadata about an attachment in Org format"
  (let ((id (elzilla//xj '("id") attachment))
        (author (elzilla//xj '("creator") attachment))
        (time (elzilla//xj '("creation_time") attachment))
        (size (elzilla//xj '("size") attachment))
        (content-type (elzilla//xj '("content_type") attachment))
        (summary (elzilla//xj '("summary") attachment)))
    (elzilla//princf "** %s\n" summary)
    (elzilla//princf "- [[elzilladl:%d][Download]]\n" id)
    (elzilla//princf "- Attached by %s on %s\n" author time)
    (elzilla//princf "- MIME Type: %s\n" content-type)
    (elzilla//princf "- Size: %d\n" size)))

(defun elzilla//print-comment (comment)
  "Prints a comment and its metadata in Org format"
  (let ((text (elzilla//xj '("text") comment))
        (author (elzilla//xj '("creator") comment))
        (time (elzilla//xj '("time") comment)))
    (princ "** Comment\n")
    (elzilla//princf "On %s, %s wrote:\n\n" time author)
    (princ "#+BEGIN_SRC text\n")
    (princ
     (mapconcat
      (lambda (x) (concat "  " x))
      (split-string text "[\r\n]" nil "[ \t]+")
      "\n"))
    (princ "\n")
    (princ "#+END_SRC\n\n")))

(defun elzilla/get-bug (bug)
  "Creates a new org-mode buffer displaying the status and comments to the given bug"
  (let* ((buffer-name (format "*elzilla: bug %s*" bug))
         (bug-buffer (get-buffer buffer-name))
         (reuse-buffer (not (null bug-buffer)))
         (status-url (elzilla//rest-url "bug/%s" (list bug) nil))
         ;; Take special care to avoid requesting the attachment body too early,
         ;; since can be much slower to retrieve than just metadata
         (attachments-url (elzilla//rest-url "bug/%s/attachment"
                                             (list bug)
                                             (list (cons "include_fields" "id,creator,creation_time,id,size,summary,content_type"))))
         (comments-url (elzilla//rest-url "bug/%s/comment" (list bug) nil)))

    (elfuture-attach
     (elfuture-join
      (elzilla//fetch-json-from-url status-url
                                    '("bugs" 0))
      (elzilla//fetch-json-from-url comments-url
                                    (list "bugs" bug "comments"))
      (elzilla//fetch-json-from-url attachments-url
                                    (list "bugs" bug)))
     (lambda (responses)
       (cl-destructuring-bind (status-map comment-vector attachments-vector) responses
         (with-output-to-temp-buffer (if reuse-buffer bug-buffer buffer-name)
             (elzilla//print-bug-long status-map)
             (elzilla//princf "* Attachments\n")
             (mapc #'elzilla//print-attachment attachments-vector)
             (elzilla//princf "* Comments\n")
             (elzilla//princf "[[elzillapost:%s][Post comment]]\n" bug)
             (mapc #'elzilla//print-comment comment-vector))
         (with-current-buffer (if reuse-buffer bug-buffer buffer-name)
           (org-mode))
         (if (not reuse-buffer)
             (pop-to-buffer buffer-name t t)))))))

(defun elzilla/prompt-bug ()
  "Prompts for a bug from the minibuffer, and displays it"
  (interactive)
  (elzilla/get-bug (read-string "Enter a bug ID: ")))

(defun elzilla//print-bug-short (bug)
  "Prints a summary of the bug in Org, including links to more detailed views"
  (let ((id (elzilla//xj '("id") bug))
        (updated (elzilla//xj '("last_change_time") bug))
        (summary (elzilla//xj '("summary") bug)))
    (elzilla//princf "** Bug %d: %s\n" id summary)
    (elzilla//princf "- [[elzilla:%d][Details]]\n" id)
    (elzilla//princf "- [[%s][Open in Browser]]\n" (elzilla//browser-link id))
    (elzilla//princf "- Last updated: %s\n" updated)))

(defun elzilla//print-bug-group (group)
  "Prints several bugs grouped by their current status"
  (let ((group-status (car group))
        (group-bugs (cdr group)))
    (elzilla//princf "* %s\n" group-status)
    (mapc #'elzilla//print-bug-short group-bugs)))

(defun elzilla/quicksearch (search)
  "Creates a new org-mode buffer which lists bugs matching the given search"
  (let* ((search-url (elzilla//rest-url "bug" nil (list (cons "quicksearch" search))))
         (buffer-name (generate-new-buffer "*elzilla: quicksearch results*")))
    (elfuture-attach
     (elzilla//fetch-json-from-url search-url '("bugs"))
     (lambda (bugs-vector)
       (seq-group-by
        (lambda (bug) (elzilla//xj '("status") bug))
        bugs-vector))
     (lambda (bug-groups)
       (with-output-to-temp-buffer buffer-name
         (mapc #'elzilla//print-bug-group bug-groups))
       (pop-to-buffer buffer-name)
       (org-mode)))))

(defun elzilla/quicksearch-prompt ()
  "Executes a search using Bugzilla quicksearch"
  (interactive)
  (elzilla/quicksearch (read-string "Enter a quicksearch: ")))

(defun elzilla/post-comment-string (bug comment)
  "Posts the given text as a new comment on the bug"
  (let* ((url (elzilla//rest-url "bug/%s/comment" (list bug) nil))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (format "{ \"comment\":%s, \"is_private\": false, \"is_markdown\": false }"
                  (json-encode-string comment))))
    (elfuture-attach
     (elfuture-retrieve-url url)
     (lambda (_)
       (message "Posted %d character comment to bug %s" (length comment) bug)))))

(defvar elzilla-comment-bug nil
  "The bug which will this buffer's contents will be posted to")

(defun elzilla//post-comment-buffer ()
  "Posts a comment to the bug number attached to the buffer"
  (interactive)
  (elzilla/post-comment-string elzilla-comment-bug
                               (buffer-substring (point-min) (point-max)))
  (kill-current-buffer))

(defvar elzilla-comment-mode-keymap
  (make-sparse-keymap)
  "The keymap used when using the comment posting minor mode")

(define-minor-mode elzilla-comment-minor-mode
  "Posts comments to Bugzilla bugs"
  :lighter "comment"
  :keymap elzilla-comment-mode-keymap
  (make-local-variable 'elzilla-comment-bug))

(define-key elzilla-comment-mode-keymap (kbd "C-c C-c") #'elzilla//post-comment-buffer)
(define-key elzilla-comment-mode-keymap (kbd "C-c C-k") #'kill-current-buffer)

(defun elzilla/post-comment (bug)
  "Opens a new buffer for posting a comment"
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*elzilla comment: %s*" bug))))
    (with-current-buffer buffer
      (markdown-mode)
      (elzilla-comment-minor-mode)
      (setq elzilla-comment-bug bug))
    (pop-to-buffer buffer)))

(defun elzilla/post-comment-prompt ()
  "Opens a new buffer for posting a comment"
  (interactive)
  (elzilla/post-comment (read-string "Enter a bug ID:")))

(defun elzilla/download-attachment (attachment)
  "Downloads the attachment to the download directory"
  (let* ((attachments-url (elzilla//rest-url "bug/attachment/%s" (list attachment) nil)))
    (elfuture-attach
     (elzilla//fetch-json-from-url attachments-url (list "attachments" attachment))
     (lambda (attachment)
       (let* ((attachment-dir (concat elzilla-attachments-dir "/"
                                     (number-to-string (elzilla//xj '("bug_id") attachment)) "/"))
              (attachment-file (concat attachment-dir
                                       (elzilla//xj '("file_name") attachment)))
              (coding-system-for-write 'binary))
         (make-directory attachment-dir t)
         (with-temp-file attachment-file
           (insert (base64-decode-string (elzilla//xj '("data") attachment))))
         (message "Wrote attachment to %s" attachment-file))))))

(defcustom org-elzilla-bug-protocol "elzilla"
  "Protocol identifier for elzilla bugs"
  :group 'org-elzilla
  :type 'string)

(defcustom org-elzilla-attachment-protocol "elzilladl"
  "Protocol identifier for elzilla attachments"
  :group 'org-elzilla
  :type 'string)

(defcustom org-elzilla-comment-protocol "elzillapost"
  "Protocol identifier for posting elzilla comments"
  :group 'org-elzilla
  :type 'string)

(org-link-set-parameters org-elzilla-bug-protocol :follow #'elzilla/get-bug)
(org-link-set-parameters org-elzilla-attachment-protocol :follow #'elzilla/download-attachment)
(org-link-set-parameters org-elzilla-comment-protocol :follow #'elzilla/post-comment)

(provide 'elzilla)
