(defpackage :vico-lib.evloop
  (:local-nicknames (:ui :vico-lib.ui) (:concurrency :vico-lib.concurrency))
  (:use :cl)
  (:export #:event
           #:queue-event #:read-event #:handle-event
           ;; key-event
           #:key-event
           #:key-name #:key-window
           ;; special-key-event
           #:standard-key-event
           #:shift #:meta #:control
           ;; special-key-event
           #:special-key-event
           ;; editor
           #:editor #:*editor*
           #:buffers #:ui-list #:event-queue #:event-loop-thread
           #:start-event-loop))
(in-package :vico-lib.evloop)

(defclass event ()
  ()
  (:documentation "TODO"))

(defgeneric queue-event (queue event)
  (:method (queue event)
    (concurrency:queue-event queue event))
  (:documentation "queues EVENT in the EVENT-LOOP instance QUEUE."))

(defgeneric read-event (queue)
  (:method (queue)
    (concurrency:read-event queue))
  (:documentation "block until an event arrives in QUEUE."))

(defgeneric handle-event (event)
  (:documentation "does whatever with EVENT. Should be specialized by subclasses of event"))

;; keyboard events

(defclass key-event (event)
  ((key-name :initarg :name
             :reader key-name
             :type keyword)
   (key-window :initarg :name
               :reader key-window
               :type ui:window))
  (:documentation "TODO"))

(defclass standard-key-event (event)
  ((shift :initarg :meta
          :reader shift
          :type boolean)
   (control :initarg :meta
            :reader control
            :type boolean)
   (meta :initarg :meta
         :reader control
         :type boolean))
  (:documentation "TODO"))

(defclass special-key-event (event) ()
  (:documentation "special keys"))

(defmethod handle-event ((event key-event))
  (ui:window-buffer (key-window event)))

;;;
;;; main TODO move out of this file
;;;

(defclass editor ()
  ((buffers :initarg :buffers
            :initform (list)
            :accessor buffers
            :type list)
   (ui-list :initarg :ui-list
            :initform (list)
            :accessor ui-list
            :type list)
   (event-queue :initarg :evqueue
                :initform (concurrency:make-event-queue)
                :accessor event-queue
                :type concurrency:event-queue)
   (editor-thread :initform concurrency:current-thread
                  :reader event-loop-thread
                  :type bt:thread)))

(defvar *editor* nil
  "EDITOR instance.")

(defmethod start-event-loop ((editor editor))
  (catch 'quit-event-loop
    (loop
      (handle-event
       (read-event
        (event-queue editor))))))
