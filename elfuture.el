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

;; Basic futures implementation. Currently only supports resolving futures
;; with single values.

(require 'ert)
(require 'url)

(cl-defstruct elfuture
  completed value callbacks)

(defvar elfuture-run-async t
  "Whether to run callbacks through the event loop or not. Should only be set to
  false for testing purposes.")

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
  "Attaches a single CALLBACK directly to FUTURE, without generating a result future"
  (if (elfuture-completed future)
      (elfuture--invoke callback (elfuture-value future))
    (setf (elfuture-callbacks future)
          (cons callback (elfuture-callbacks future)))))

(defun elfuture--raw-resolve (future value)
  "Resolves FUTURE with VALUE if FUTURE."
  (setf (elfuture-completed future) t)
  (setf (elfuture-value future) value)
  (mapc (lambda (cb)
          (elfuture--invoke cb value))
        (reverse (elfuture-callbacks future))))

(defun elfuture-resolve (future value)
  "Resolves FUTURE with VALUE if FUTURE is not already resolved."
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
  "Attaches a single CALLBACK to FUTURE, and uses the result
  to resolve a new future which is returned."
  (let* ((chain-future (elfuture-new))
         (chain-callback
          (lambda (value)
            (elfuture-resolve chain-future (funcall callback value)))))

    (elfuture--raw-attach future chain-callback)
    chain-future))

(defun elfuture-attach (future &rest callbacks)
  "Chains CALLBACKS onto each other, starting from FUTURE.

  When FUTURE finishes, it invokes the first entry in
  CALLBACKS. Then, the return value from that function
  is fed into the second entry in CALLBACKS, and so on.
  The last entry resolves a new callback which is returned."
  (seq-reduce #'elfuture--attach1
              callbacks
              future))

(defun elfuture-retrieve-url (url)
  "Retrieves URL using url-retrieve and returns a future which completes with
   the text content of the response."
  (let ((future (elfuture-new)))
    (url-retrieve url
                  (lambda (_)
                    (elfuture-resolve future (current-buffer)))
                  '()
                  t)
    future))

(ert-deftest elfuture--test-new ()
  "Test the properties of a new future"
  (let ((elfuture-run-async nil)
        (future (elfuture-new)))
    (should-not (elfuture-completed future))
    (should (null (elfuture-value future)))
    (should (null (elfuture-callbacks future)))))

(ert-deftest elfuture--test-resolve ()
  "Tests the properties of a resolved future"
  (let ((elfuture-run-async nil)
        (future (elfuture-new)))
    (elfuture-resolve future 42)
    (should (elfuture-completed future))
    (should (= 42 (elfuture-value future)))
    (should (null (elfuture-callbacks future)))))

(ert-deftest elfuture--test-attach-one ()
  "Tests that resolving a future invokes the attachment"
  (let* ((elfuture-run-async nil)
         (future (elfuture-new))
         (x nil))
    (elfuture-attach future
                     (lambda (value)
                       (setq x value)))
    (elfuture-resolve future 42)
    (should (= 42 x))))

(ert-deftest elfuture--test-attach-order ()
  "Tests that resolving a future invokes the attachments in order"
  (let ((elfuture-run-async nil)
        (future (elfuture-new))
        (x nil))
    (elfuture-attach future
                     (lambda (_)
                       (setq x (cons 1 x))))
    (elfuture-attach future
                     (lambda (_)
                       (setq x (cons 2 x))))
    (elfuture-resolve future 42)
    (should (equal '(2 1) x))))

(ert-deftest elfuture--test-attach-chain ()
  "Tests that resolving a future cascades"
  (let* ((elfuture-run-async nil)
         (future (elfuture-new))
         (x nil)
         (y nil)
         (future-attach
          (elfuture-attach
           (elfuture-attach future (lambda (value)
                                     (setq x value)
                                     (* x 2)))
           (lambda (value)
             (setq y value)
             (* y 2)))))
    (elfuture-resolve future 42)
    (should (= x 42))
    (should (= y 84))
    (should (elfuture-completed future))
    (should (= 42 (elfuture-value future)))
    (should (elfuture-completed future-attach))
    (should (= 168 (elfuture-value future-attach)))))

(ert-deftest elfuture--test-attach-nested ()
  "Tests that a callback returning a future is attached to the original future
   which elfuture-attach returned."
  (let* ((elfuture-run-async nil)
         (future (elfuture-new))
         (future2 (elfuture-new))
         (future3
          (elfuture-attach
           future2
           (lambda (value) future2))))
    (elfuture-resolve future 42)
    (should (elfuture-completed future))
    (should (= 42 (elfuture-value future)))
    (should-not (elfuture-completed future2))
    (should-not (elfuture-completed future3))
    (elfuture-resolve future2 84)
    (should (elfuture-completed future2))
    (should (= 84 (elfuture-value future2)))
    (should (elfuture-completed future3))
    (should (= 84 (elfuture-value future3)))))

(provide 'elfuture)
