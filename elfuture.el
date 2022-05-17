;;; elzilla.el --- Bugzilla interface for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 Chris Marchetti
;;
;; Author: Chris Marchetti <adamew123456@gmail.com>
;; Version: 0.2
;; Package-Requires: (url)
;; Keywords: futures, async
;;
;;; License:
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package provides a basic futures implementation that can be used for
;; managing async operations.
;;
;;; Code:

(require 'url)

(cl-defstruct elfuture
  completed value callbacks)

(defvar elfuture-run-async t
  "Whether to run callbacks through the event loop or not. Should only be set to
false for testing purposes.")

;;;###autoload
(defun elfuture-new ()
  "Creates a new future, which may be attached to a callback or awaited."
  (make-elfuture :completed nil
                 :value nil
                 :callbacks '()))

(defun elfuture--invoke (callback value)
  "Invokes a future callback with the given VALUE."
  (if elfuture-run-async
      (run-at-time 0 nil callback value)
    (funcall callback value)))

(defun elfuture--raw-attach (future callback)
  "Attaches a single CALLBACK directly to FUTURE, without generating a result
future. Outside of the elfuture internals, this is not desirable and `elfuture-attach'
should be used instead."
  (if (elfuture-completed future)
      (elfuture--invoke callback (elfuture-value future))
    (setf (elfuture-callbacks future)
          (cons callback (elfuture-callbacks future)))))

(defun elfuture--raw-resolve (future value)
  "Resolves FUTURE directly with VALUE if FUTURE is not already resolved.

Does not recursively resolve VALUE if VALUE is itself a future. Use
`elfuture-resolve' if that is needed."
  (setf (elfuture-completed future) t)
  (setf (elfuture-value future) value)
  (mapc (lambda (cb)
          (elfuture--invoke cb value))
        (reverse (elfuture-callbacks future))))

(defun elfuture-resolve (future value)
  "Resolves FUTURE with VALUE if FUTURE is not already resolved.

This recursively resolves VALUE if VALUE itself is a future. In that case,
FUTURE will be resolved (with the same value) as soon as VALUE itself is
resolved."
  (cond
   ((elfuture-completed future) nil)

   ((elfuture-p value)
    (elfuture--raw-attach
     value
     (lambda (inner) (elfuture-resolve future inner))))

   (t
    (setf (elfuture-completed future) t)
    (setf (elfuture-value future) value)
    (mapc (lambda (cb)
            (elfuture--invoke cb value))
          (reverse (elfuture-callbacks future))))))

(defun elfuture--attach1 (future callback)
  "Attaches a single CALLBACK to FUTURE, and uses the result to resolve a new
future which is returned.

In most cases `elfuture-attach' should be used because it generalizes this
to multiple callbacks for easy chaining:

  (elfuture-attach
   the-future
   (lambda (x) (* x 2))
   (lambda (y) (* y 3)))
  ;; Resolving the-future with 1 will resolve 6 on the callback returned by
  ;; elfuture-attach"
  (let* ((chain-future (elfuture-new))
         (chain-callback
          (lambda (value)
            (elfuture-resolve chain-future (funcall callback value)))))

    (elfuture--raw-attach future chain-callback)
    chain-future))

;;;###autoload
(defun elfuture-attach (future &rest callbacks)
  "Chains CALLBACKS onto each other, starting from FUTURE.

When FUTURE finishes, it invokes the first entry in CALLBACKS. Then, the
return value from that function is fed into the second entry in CALLBACKS, and
so on. The last entry resolves a new future which is returned."
  (seq-reduce #'elfuture--attach1
              callbacks
              future))

;;;###autoload
(defun elfuture-join (&rest futures)
  "Creates a single future which is resolved with the combined values from FUTURES.

When all of the FUTURES resolve, their values are combined into a single list which
is used to resolve the returned future."
  (let ((join-future (elfuture-new)))
    (mapc
     (lambda (future)
       (elfuture--raw-attach
        future
        (lambda (_)
          (if (seq-every-p #'elfuture-completed futures)
              (elfuture-resolve join-future
                                (mapcar #'elfuture-value futures))))))
     futures)
    join-future))

;;;###autoload
(defun elfuture-retrieve-url (url)
  "Retrieves URL using url-retrieve and returns a future which completes with
the buffer containing the response."
  (let ((future (elfuture-new)))
    (url-retrieve url
                  (lambda (_)
                    (elfuture-resolve future (current-buffer)))
                  '()
                  t)
    future))


(provide 'elfuture)
;;; elfuture.el ends here
