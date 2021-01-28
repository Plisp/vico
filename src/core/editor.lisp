;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editor event loop (main)
;;
;;

(defpackage :vico-core.command-loop
  (:use :cl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:ui          :vico-core.ui))
  (:export #:command
           #:queue-command #:read-command #:handle-command

           #:editor #:*editor*
           #:buffers
           #:frontends
           #:command-queue #:command-loop-thread
           #:start-editor-loop #:quit-editor-loop))
(in-package :vico-core.command-loop)

(defclass command ()
  ()
  (:documentation "TODO"))

(defgeneric queue-command (queue command)
  (:method (queue command)
    (concurrency:queue-event queue command))
  (:documentation "queues COMMAND in the COMMAND-LOOP instance QUEUE."))

(defun read-command (queue &key timeout)
  (let ((command (concurrency:read-event queue :timeout timeout)))
    (typecase command
      (symbol (uiop:ensure-function command))
      (function command)
      (list (values (first command) (rest command)))
      (otherwise command))))

(defgeneric handle-command (command context)
  (:method ((command function) context)
    (apply command context))
  (:documentation
   "does whatever with COMMAND. Should be specialized by subclasses of command"))

(defclass editor ()
  ((buffers :initarg :buffers
            :initform (list)
            :accessor buffers
            :type list)
   (frontends :initarg :frontends
              :initform (list)
              :accessor frontends
              :type list)
   (command-queue :initform (concurrency:make-event-queue)
                  :accessor command-queue
                  :type concurrency:event-queue)
   (command-loop-thread :initform concurrency:current-thread
                        :reader command-loop-thread
                        :type bt:thread)
   (arg :initform 1
        :accessor editor-arg)))

(defvar *editor* nil
  "EDITOR instance.")

;; TODO handle timers in command loop (or use trivial-timers)
;; TODO handling for quits (will need to interrupt-proof buffer operations)
;; TODO may need to switch to priority queue for command priorities
(defmethod start-editor-loop ((editor editor))
  (catch 'quit-editor-loop
    (loop
      (block handling-command
        (restart-case
            (multiple-value-bind (command context)
                (read-command (command-queue editor))
              (handle-command command context))
          (never-gonna-give-you-up ()
            (return-from handling-command)))))))

;; must be called from within the dynamic extent of `start-editor-loop`
(defmethod quit-editor-loop ((editor editor))
  (throw 'quit-editor-loop :quit))
