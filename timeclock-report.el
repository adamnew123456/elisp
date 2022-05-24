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

(defun timeclock-report-reload (&rest args)
  "Reloads the timeclock file and regnerates the report in the current buffer."
  (interactive)
  (if (not (derived-mode-p 'timeclock-report-mode))
      (user-error "Cannot reload report in a non-timeclock buffer"))

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
  (if (not (derived-mode-p 'timeclock-report-mode))
      (user-error "Cannot reload report in a non-timeclock buffer"))
  (call-interactively 'timeclock-in)
  (timeclock-report-reload))

(defun timeclock-out-and-reload ()
  "Clocks out via TIMECLOCK-OUT and reloads the report."
  (interactive)
  (if (not (derived-mode-p 'timeclock-report-mode))
      (user-error "Cannot reload report in a non-timeclock buffer"))
  (call-interactively 'timeclock-out)
  (timeclock-report-reload))

(provide 'timeclock-report)
