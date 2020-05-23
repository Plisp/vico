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
           #:window-name
           #:window-ui
           #:window-x #:window-y #:window-width #:window-height
           #:window-buffer
           #:move-window #:resize-window
           #:window-top-line #:scroll-window
           #:window-point
           #:move-point #:move-point-lines))
(in-package :vico-core.ui)

(defclass ui ()
  ((ui-thread :initarg :thread
              :initform (concurrency:current-thread)
              :accessor ui-thread
              :type concurrency:thread))
  (:documentation "To be subclassed by all user frontends."))

(define-condition vico-protocol-unimplemented-error (conditions:vico-error)
  ((ui-type :initarg :type
            :reader ui-type)
   (ui-object :initarg :object
              :reader ui-object)
   (ui-function :initarg :function
                :reader ui-function))
  (:report (lambda (condition stream)
             (format stream "The object ~A does not implement the ~A (~A protocol)"
                     (ui-object condition)
                     (ui-function condition)
                     (ui-type condition)))))

(define-condition vico-ui-unimplemented-error (vico-protocol-unimplemented-error)
  ((ui-type :initform 'ui
            :reader ui-type)))

(defmacro define-ui-protocol (name (&rest arglist) &optional documentation)
  `(defgeneric ,name (,@arglist)
     (:method (,@arglist)
       (declare (ignorable ,@(loop :for arg in arglist
                                   :unless (position arg lambda-list-keywords)
                                     :collect arg)))
       (error 'vico-ui-unimplemented-error
              :function ',name
              :object ,(if (and (listp name) (eq (first name) 'setf))
                           (second arglist)
                           (first arglist))))
     ,(list :documentation (or documentation "undocumented"))))

(define-ui-protocol start (ui))
(define-ui-protocol quit (ui))

(define-ui-protocol windows (ui) "Returns a list of windows under the ui instance UI.")
(define-ui-protocol (setf windows) (new-value ui))
(define-ui-protocol focused-window (ui))
(define-ui-protocol (setf focused-window) (new-value ui))

(define-ui-protocol width (ui))
(define-ui-protocol (setf width) (new-value ui))
(define-ui-protocol height (ui))
(define-ui-protocol (setf height) (new-value ui))

(define-ui-protocol redisplay (ui &key force-p)
  "Causes changed areas in UI to be redisplayed. May be called from the editor thread.
If FORCE-P is non-null, redisplay everything unconditionally.")

;; maybe remove later along with window :after redisplay hook (leave it up to the frontend?)
(defmethod (setf windows) :after (new-value ui) (redisplay ui))
(defmethod (setf focused-window) :after (new-value ui) (redisplay ui))
(defmethod (setf width) :after (new-value ui) (redisplay ui))
(defmethod (setf height) :after (new-value ui) (redisplay ui))
