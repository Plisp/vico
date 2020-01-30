;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; user interface protocol
;;
;;

(defpackage :vico-core.ui
  (:use :cl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:conditions :vico-core.conditions))
  (:export #:ui
           #:ui-thread
           #:start #:quit
           #:windows #:focused-window
           #:width #:height
           #:redisplay

           #:window #:make-window
           #:window-ui
           #:window-x #:window-y #:window-width #:window-height
           #:window-name
           #:window-buffer
           #:window-string-width #:window-line-height
           #:move-window
           #:resize-window
           #:raise-window
           #:lower-window
           #:top-line))
(in-package :vico-core.ui)

(defclass ui ()
  ((ui-thread :initarg :thread
              :initform (concurrency:current-thread)
              :reader ui-thread
              :type concurrency:thread))
  (:documentation "To be subclassed by all user frontends."))

(define-condition vico-protocol-unimplemented-error (conditions:vico-error)
  ((ui-type :initarg :type
            :reader ui-type)
   (ui-object :initarg :object
              :reader ui-object))
  (:report (lambda (condition stream)
             (format stream "The object ~A does not implement this ~A protocol."
                     (ui-object condition)
                     (ui-type condition)))))

(defmacro define-protocol (name (&rest arglist) &optional documentation)
  `(defgeneric ,name (,@arglist)
     (:method (,@arglist)
       (declare (ignorable ,@(loop :for arg in arglist
                                   :unless (position arg lambda-list-keywords)
                                     :collect it)))
       (error 'vico-protocol-unimplemented-error
              :type ',(if (and (listp name) (eq (first name) 'setf))
                          (second arglist)
                          (first arglist))
              :object ,(if (and (listp name) (eq (first name) 'setf))
                           (second arglist)
                           (first arglist))))
     ,(list :documentation (or documentation "undocumented"))))

(define-protocol start (ui))
(define-protocol quit (ui))

(define-protocol windows (ui) "Returns a list of windows under the ui instance UI.")
(define-protocol (setf windows) (new-value ui))
(define-protocol focused-window (ui))
(define-protocol (setf focused-window) (new-value ui))

(define-protocol width (ui))
(define-protocol (setf width) (new-value ui))
(define-protocol height (ui))
(define-protocol (setf height) (new-value ui))

(define-protocol redisplay (ui &key force-p)
  "Causes changed areas in UI to be redisplayed. May be called from the editor thread.
If FORCE-P is non-null, redisplay everything unconditionally.")
