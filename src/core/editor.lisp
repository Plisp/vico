;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editor event loop (main)
;;
;;

(defpackage #:vico-core.editor
  (:use :cl)
  (:local-nicknames )
  (:export #:frontend
           #:running?
           #:width #:height
           #:layout))
(in-package #:vico-core.editor)

(defclass frontend ()
  ((event-queue :initform (safe-queue:make-queue :name "VICO EVENT QUEUE")
                :reader events
                :type safe-queue:queue)
   (running? :initform nil
             :accessor running?
             :type boolean)
   (width :initarg :w
          :accessor width)
   (height :initarg :h
           :accessor height)
   (layout :initarg :layout ; NO WINDOW OBJECTS
           :accessor layout)
   ))

(defgeneric handle-event (event))

(defun push-event (event frontend)
  (safe-queue:enqueue event (events frontend)))
