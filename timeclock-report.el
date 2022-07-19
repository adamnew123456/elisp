;;; elisp/timeclock-report/timeclock-report.el -*- lexical-binding: t; -*-
;; Copyright 2022, Chris Marchetti

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
(require 'timeclock)

;;;;; Reporting functions

;;;###autoload
(defun timeclock-report ()
    "Opens a timeclock report buffer"
    (interactive)
    (with-current-buffer (get-buffer-create "*timeclock-report*")
      (pop-to-buffer (current-buffer))
      (timeclock-report-mode)))

(defvar timeclock-report-mode-map)

(define-derived-mode timeclock-report-mode special-mode "Timeclock-Report"
  "A mode for viewing reports from timeclock."
  (setq-local revert-buffer-function #'timeclock-report-reload)
  (timeclock-report-reload))

(define-key timeclock-report-mode-map (kbd "C-c i") #'timeclock-in-and-reload)
(define-key timeclock-report-mode-map (kbd "C-c o") #'timeclock-out-and-reload)
(define-key timeclock-report-mode-map (kbd "C-c r") #'timeclock-report-reload)
(define-key timeclock-report-mode-map (kbd "C-c e") #'timeclock-edit-and-reload)
(define-key timeclock-report-mode-map (kbd "C-c a") #'timeclock-archive-mode)

(defun timeclock-report-check-buffer ()
  "Validates that the current buffer is a timeclock-report buffer."
  (if (not (derived-mode-p 'timeclock-report-mode))
      (user-error "Cannot perform this operation in a non-timeclock buffer")))

(defun timeclock-report-reload (&rest _args)
  "Reloads the timeclock file and regnerates the report in the current buffer."
  (interactive)
  (timeclock-report-check-buffer)
  (timeclock-reread-log)
  (with-silent-modifications
    (erase-buffer)
    (insert (propertize (timeclock-status-string) 'face 'bold))
    (insert "\n\n")
    (let ((per-project-status (caddr (timeclock-log-data))))
      (mapc #'timeclock-report-task per-project-status))
    (goto-char 1)))

(defun timeclock-report-task (log-entry)
  "Inserts an entry for the report for the given LOG-ENTRY"
  (let ((task (car log-entry))
        (events (cdr log-entry))
        (total-time-sec 0))
    (insert (propertize task 'face 'underline))
    (insert "\n")
    (mapc (lambda (event)
            (let* ((start-time (time-convert (nth 0 event) 'integer))
                   (end-time (time-convert (nth 1 event) 'integer))
                   (duration (- end-time start-time)))
              (insert (format "    %s (from %s)\n"
                              (timeclock-seconds-to-string duration)
                              (format-time-string "%Y-%m-%d %H:%M" start-time)))
              (setq total-time-sec (+ total-time-sec duration))))
          events)
    (insert (format "%s total\n\n"
                    (timeclock-seconds-to-string total-time-sec)))))

(defun timeclock-in-and-reload ()
  "Clocks in via TIMECLOCK-IN and reloads the report."
  (interactive)
  (timeclock-report-check-buffer)
  (call-interactively 'timeclock-in)
  (timeclock-report-reload))

(defun timeclock-out-and-reload ()
  "Clocks out via TIMECLOCK-OUT and reloads the report."
  (interactive)
  (timeclock-report-check-buffer)
  (call-interactively 'timeclock-out)
  (timeclock-report-reload))

(defun timeclock-edit-and-reload ()
  "Opens the timeclock file in another buffer and hooks it to update this buffer on save."
  (interactive)
  (timeclock-report-check-buffer)
  (let ((report-buffer (current-buffer))
        (edit-buffer (find-file timeclock-file)))
    (with-current-buffer edit-buffer
      (add-hook 'after-save-hook
                (lambda ()
                  (with-current-buffer report-buffer
                    (timeclock-report-reload)))
                nil
                t))
    (pop-to-buffer edit-buffer)))

;;;;; Archiving functions

(defvar timeclock-archive-mode-map)

(define-derived-mode timeclock-archive-mode special-mode "Timeclock-Archive"
  "A mode for archiving timeclock entries."
  (if (derived-mode-p 'timeclock-report-mode)
      (timeclock-report-mode))

  (setq-local revert-buffer-function #'timeclock-archive-abort)
  (timeclock-archive-load))

(define-key timeclock-archive-mode-map (kbd "C-c C-c") #'timeclock-archive-toggle)
(define-key timeclock-archive-mode-map (kbd "C-c c") #'timeclock-archive-commit)
(define-key timeclock-archive-mode-map (kbd "C-c a") #'timeclock-archive-abort)

(defun timeclock-archive-load ()
  "Inserts a list of tasks and sets their associated archive markers."
  (interactive)
  (timeclock-archive-check-buffer)
  (timeclock-reread-log)
  (with-silent-modifications
    (erase-buffer)
    (let ((per-project-status (caddr (timeclock-log-data))))
      (mapc #'timeclock-archive-task per-project-status))
    (goto-char 1)))

(defun timeclock-archive-task (log-entry)
  "Inserts an entry into the archive buffer for the given LOG-ENTRY."
  (let ((task (car log-entry))
        (events (cdr log-entry)))
    (insert (propertize (concat task "\n")
                        'timeclock-type 'task
                        'timeclock-task task))
    (mapc (lambda (event)
            (let* ((start-time (time-convert (nth 0 event) 'integer))
                   (end-time (time-convert (nth 1 event) 'integer))
                   (duration (- end-time start-time))
                   (entry-text (format "    %s (from %s)\n"
                                       (timeclock-seconds-to-string duration)
                                       (format-time-string "%Y-%m-%d %H:%M" start-time))))
              (insert (propertize entry-text
                                  'face 'success
                                  'timeclock-type 'entry
                                  'timeclock-task task
                                  'timeclock-entry-included t
                                  'timeclock-entry-start (nth 0 event)
                                  'timeclock-entry-end (nth 1 event)))))
          events)
    (insert (propertize "\n" 'timeclock-type 'end))))

(defun timeclock-archive-type-at (pos)
  "Gets the type of archive object at POS, or nil if there is none."
  (let ((prps (text-properties-at (point))))
    (plist-get prps 'timeclock-type)))

(defun timeclock-archive-fontify (start end)
  "Updates the font properties on the range START to END, using the include status."
  (if (not (eq (timeclock-archive-type-at start) 'entry))
      (error "No archive entry at point"))

  (let* ((prps (text-properties-at start))
         (included (plist-get prps 'timeclock-entry-included)))
    (with-silent-modifications
      (put-text-property start end 'face (if included 'success 'error)))))

(defun timeclock-archive-check-buffer ()
  "Validates that the current buffer is a timeclock-archive buffer."
  (if (not (derived-mode-p 'timeclock-archive-mode))
      (user-error "Cannot perform this operation in a non-archive buffer")))

(defun timeclock-archive-toggle ()
  "Toggles the currently selected archive entry."
  (interactive)
  (timeclock-archive-check-buffer)
  (pcase (timeclock-archive-type-at (point))
    ('nil
     (message "No archive task or entry at point"))

    ('entry
     (let* ((prps (text-properties-at (point)))
            (included (plist-get prps 'timeclock-entry-included)))
       (with-silent-modifications
         (put-text-property (line-beginning-position)
                            (line-end-position)
                            'timeclock-entry-included (not included)))
       (timeclock-archive-fontify (line-beginning-position)
                                  (line-end-position))))

    ('task
     (while (not (eq (timeclock-archive-type-at (point)) 'end))
       (forward-line)
       (timeclock-archive-toggle)))))

(defun timeclock-archive-save-tasks-to-file (file tasks)
  "Formats the list of TASKS in timeclock format and writes them to FILE."
  (with-temp-buffer
    (erase-buffer)
    (mapc (lambda (entry)
            (let ((timestamp (elt entry 0))
                  (task (elt entry 1))
                  (op (elt entry 2)))
              (insert (if (eq op 'in) "i " "o "))
              (insert (format-time-string "%Y/%m/%d %H:%M:%S " timestamp))
              (if (eq op 'in)
                  (insert task))
              (insert "\n")))

          (sort tasks
                (lambda (a b)
                  (let ((a-time (time-convert (elt a 0) 'integer))
                        (b-time (time-convert (elt b 0) 'integer)))
                    (< a-time b-time)))))

    (write-file file)
    (kill-buffer)))

(defun timeclock-archive-commit ()
  "Moves all non-selected entries from the timeclock file to the archive."
  (interactive)
  (timeclock-archive-check-buffer)
  (goto-char (point-min))
  (let ((new-task-list '())
        (old-task-list '()))
    (while (< (point) (point-max))
      (if (eq 'entry (timeclock-archive-type-at (point)))
          (let* ((prps (text-properties-at (point)))
                 (included (plist-get prps 'timeclock-entry-included))
                 (task (plist-get prps 'timeclock-task))
                 (start (plist-get prps 'timeclock-entry-start))
                 (end (plist-get prps 'timeclock-entry-end)))
            (if included
                (progn
                  (setq new-task-list (cons (vector start task 'in) new-task-list))
                  (setq new-task-list (cons (vector end task 'out) new-task-list)))
                (progn
                  (setq old-task-list (cons (vector start task 'in) old-task-list))
                  (setq old-task-list (cons (vector end task 'out) old-task-list))))))
      (forward-line))

    (timeclock-archive-save-tasks-to-file timeclock-file new-task-list)
    (timeclock-archive-save-tasks-to-file (concat timeclock-file ".archive")
                                          old-task-list))
  (timeclock-archive-mode)
  (timeclock-report-mode))

(defun timeclock-archive-abort (&rest _args)
  "Kills the buffer and returns to timeclock-report mode."
  (interactive)
  (timeclock-archive-check-buffer)
  (timeclock-archive-mode)
  (timeclock-report-mode))

(provide 'timeclock-report)
