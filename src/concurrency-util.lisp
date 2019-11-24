(defpackage :vico-lib.concurrency
  (:use :cl)
  (:shadowing-import-from :vico-core
   :length :char :subseq)
  (:export
   :current-thread
   :without-interrupts :with-local-interrupts
   :event-queue :make-event-queue :queue-event :read-event))
(in-package :vico-lib.concurrency)

(define-symbol-macro current-thread (bt:current-thread))

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

;;;
;;; event queue
;;;

(deftype event-queue ()
  'safe-queue:mailbox)

(defun make-event-queue (&key initial-contents)
  (safe-queue:make-mailbox :name "EVENT QUEUE"
                           :initial-contents initial-contents))

(defun queue-event (queue event)
  (safe-queue:mailbox-send-message queue event))

(defun read-event (queue)
  (safe-queue:mailbox-receive-message queue))
