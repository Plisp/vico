;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editor event loop (main)
;;
;; TODO write libev style event loop and screw threading
;;

(defpackage :vico-lib.evloop
  (:use :cl :alexandria)
  (:local-nicknames (:concurrency :vico-lib.concurrency)
                    (:ui          :vico-lib.ui))
  (:export #:event
           #:queue-event #:read-event #:handle-event

           #:editor #:*editor*
           #:buffers
           #:frontends
           #:keybinds
           #:event-queue #:event-loop-thread
           #:start-editor-loop #:quit-editor-loop))
(in-package :vico-lib.evloop)

(defclass event ()
  ((context :initarg :context
            :initform (list)
            :reader event-context
            :type list))
  (:documentation "TODO"))

(defgeneric queue-event (queue event)
  (:method (queue event)
    (concurrency:queue-event queue event))
  (:documentation "queues EVENT in the EVENT-LOOP instance QUEUE."))

(defun read-event (queue &key timeout)
  (concurrency:read-event queue :timeout timeout))

(defgeneric handle-event (event)
  (:method ((event null)))
  (:documentation "does whatever with EVENT. Should be specialized by subclasses of event"))

;;; main TODO move out of this file

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
   (event-loop-thread :initform (concurrency:current-thread)
                      :reader event-loop-thread
                      :type concurrency:thread)))

(defvar *editor* nil
  "EDITOR instance.")

;; TODO handle timers in event loop
;; TODO handling for quits
(defmethod start-editor-loop ((editor editor))
  (catch 'quit-editor-loop
    (loop
      (handle-event
       (read-event
        (event-queue editor))))))

;; must be called from within the dynamic extent of `start-editor-loop`
(defmethod quit-editor-loop ((editor editor))
  (throw 'quit-editor-loop :quit))
