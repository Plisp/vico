;;;;
;;;
;; editor event loop (main)
;; TODO write libev style event loop and screw threading
;;;
;;;;

(defpackage :vico-lib.evloop
  (:use :cl :alexandria)
  (:local-nicknames (:ui :vico-lib.ui) (:concurrency :vico-lib.concurrency))
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
           #:buffers #:frontends #:keybinds #:event-queue #:event-loop-thread
           #:start-event-loop #:quit-event-loop))
(in-package :vico-lib.evloop)

(defclass event ()
  ((context :initarg :context
            :initform nil
            :reader event-context))
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

(defmethod handle-event ((event key-event))
  (funcall (assoc-value (keybinds (ui:window-buffer (key-window event))) event)
           (event-context event)))

;;;
;;; main TODO move out of this file
;;;

(defclass editor ()
  ((buffers :initarg :buffers
            :initform (list)
            :accessor buffers
            :type list)
   (frontends :initarg :frontends
              :initform (list)
              :accessor frontends
              :type list)
   (keybinds :initarg :keybinds
             :initform (list)
             :reader keybinds ; should use API for modification
             :type list) ; alist
   (event-queue :initarg :evqueue
                :initform (concurrency:make-event-queue)
                :accessor event-queue
                :type concurrency:event-queue)
   (event-loop-thread :initform (concurrency:current-thread)
                      :accessor event-loop-thread ;KLUDGE
                      :type concurrency:thread)))

(defvar *editor* nil
  "EDITOR instance.")

;; TODO handle timers in event loop
(defmethod start-event-loop ((editor editor))
  (catch 'quit-event-loop
    (loop
      (handle-event
       (read-event
        (event-queue editor))))))
