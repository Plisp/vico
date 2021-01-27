;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editor event loop (main)
;;
;;

(defpackage :vico-core.evloop
  (:use :cl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:ui          :vico-core.ui))
  (:export #:event
           #:queue-event #:read-event #:handle-event

           #:log-event

           #:editor #:*editor*
           #:buffers
           #:frontends
           #:event-queue #:event-loop-thread
           #:start-editor-loop #:quit-editor-loop
           #:editor-arg #:*editor-arg*)) ;TODO rename
(in-package :vico-core.evloop)

(defclass event ()
  ()
  (:documentation "TODO"))

(defgeneric queue-event (queue event)
  (:method (queue event)
    (concurrency:queue-event queue event))
  (:documentation "queues EVENT in the EVENT-LOOP instance QUEUE."))

(defun read-event (queue &key timeout)
  (concurrency:read-event queue :timeout timeout))

(defgeneric handle-event (event)
  (:method ((event null)))
  (:documentation
   "does whatever with EVENT. Should be specialized by subclasses of event"))

;; note: only designed to be subclassed/specialised - never multiple instances
(defclass editor ()
  ((buffers :initarg :buffers
            :initform (list)
            :accessor buffers
            :type list)
   (frontends :initarg :frontends
              :initform (list)
              :accessor frontends
              :type list)
   (event-queue :initarg :evqueue
                :initform (concurrency:make-event-queue)
                :accessor event-queue
                :type concurrency:event-queue)
   (event-loop-thread :initform concurrency:current-thread
                      :reader event-loop-thread
                      :type bt:thread)
   (arg :initform 1
        :accessor editor-arg)))

;;XXX should be per frontend
(define-symbol-macro *editor-arg* (editor-arg *editor*))

(defvar *editor* nil
  "EDITOR instance.")

;; TODO handle timers in event loop
;; TODO handling for quits
;; TODO may need to switch to priority queue for event priorities
(defmethod start-editor-loop ((editor editor))
  (catch 'quit-editor-loop
    (loop
      (block handling-event
        (restart-case
            (handle-event
             (read-event
              (event-queue editor)))
          (never-gonna-give-you-up ()
            (return-from handling-event)))))))

;; must be called from within the dynamic extent of `start-editor-loop`
(defmethod quit-editor-loop ((editor editor))
  (throw 'quit-editor-loop :quit))

(defclass log-event (event)
  ((message :initarg :log-message
            :accessor log-message
            :type string)))

(defmethod handle-event ((event log-event))
  (format t "~&[log]:~a~&" (log-message event))
  (finish-output))

(declaim (notinline log-event))
(defun log-event (message)
  (when (member :slynk *features*)
    (queue-event (event-queue *editor*)
                 (make-instance 'log-event :log-message (princ-to-string message))))
  message)
