(defpackage :vico-core.concurrency
  (:use :cl)
  (:export #:current-thread
           #:without-interrupts #:with-local-interrupts
           #:event-queue #:make-event-queue
           #:queue-event #:read-event))
(in-package :vico-core.concurrency)

(define-symbol-macro current-thread (bt:current-thread))

;;; interrupts

(defmacro without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts
    #+ccl ccl:without-interrupts
    #+ecl ext:without-interrupts
    #-(or sbcl ccl ecl) progn
    ,@body))

(defmacro with-local-interrupts (&body body)
  `(#+sbcl sb-sys:with-local-interrupts
    #+ccl ccl:with-interrupts-enabled
    #+ecl ext:with-local-interrupts
    #-(or sbcl ccl ecl) progn
    ,@body))

;;; event queue

(deftype event-queue ()
  'safe-queue:mailbox)

(defun make-event-queue (&key initial-contents)
  (safe-queue:make-mailbox :name "EVENT QUEUE"
                           :initial-contents initial-contents))

(defun queue-event (queue event)
  (safe-queue:mailbox-send-message queue event))

(defun read-event (queue &key timeout)
  (safe-queue:mailbox-receive-message queue :timeout timeout))
