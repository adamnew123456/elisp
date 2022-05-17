;;; elisp/elfuture/elfuture-test.el -*- lexical-binding: t; -*-

(require 'ert)

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

(provide 'elfuture-test)
