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

(defun elfuture-attach (future &rest callbacks)
  "Chains CALLBACKS onto each other, starting from FUTURE.

When FUTURE finishes, it invokes the first entry in CALLBACKS. Then, the
return value from that function is fed into the second entry in CALLBACKS, and
so on. The last entry resolves a new future which is returned."
  (seq-reduce #'elfuture--attach1
              callbacks
              future))

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
           (lambda (_) future2))))
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

(ert-deftest elfuture--test-join ()
  "Tests that a joined future resolves with all values after all input futures
have resolved."
  (let* ((elfuture-run-async nil)
         (future (elfuture-new))
         (future2 (elfuture-new))
         (future3 (elfuture-new))
         (joined-future (elfuture-join future future2 future3)))
    (should-not (elfuture-completed joined-future))
    (elfuture-resolve future 1)
    (should-not (elfuture-completed joined-future))
    (elfuture-resolve future2 2)
    (should-not (elfuture-completed joined-future))
    (elfuture-resolve future3 3)
    (should (elfuture-completed joined-future))
    (should (equal '(1 2 3) (elfuture-value joined-future)))))

(provide 'elfuture)
