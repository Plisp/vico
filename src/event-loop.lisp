(in-package :vico-lib)

(defvar *editor* nil
  "Editor instance - only 1 should be around at any time. Holds all state.")

(defclass editor ()
  ((buffers :initarg :buffers
            :initform (list)
            :type list)
   (frontends :initarg :frontends
              :initform (list)
              :type list)
   (event-queue :initarg :evqueue
                :initform (make-event-queue)
                :type event-queue)
   (event-loop-thread :initform (bt:current-thread)
                      :type bt:thread)))

(deftype event-queue ()
  'safe-queue:mailbox)

(defun make-event-queue (&key initial-contents)
  (safe-queue:make-mailbox :name "EVENT QUEUE"
                           :initial-contents initial-contents))

(defun send-event (queue event)
  (safe-queue:mailbox-send-message queue event))

(defun get-event (queue)
  (safe-queue:mailbox-receive-message queue))

(defun start-event-loop (editor)
  ;;(set-thread-pool-size)
  (catch 'quit
    (let ((evqueue (slot-value editor 'event-queue)))
      (loop (get-event evqueue)))))
