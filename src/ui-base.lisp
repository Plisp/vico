;;;;
;;;
;;; user interface protocol
;;;
;;;;

(defpackage :vico-lib.ui
  (:use :cl)
  (:local-nicknames (:concurrency :vico-lib.concurrency)
                    (:conditions :vico-core.conditions))
  (:export #:ui #:ui-thread #:windows
           #:window #:make-window
           #:window-name
           #:window-buffer
           #:redisplay-window
           #:window-x #:window-y #:window-width #:window-height
           #:window-string-width #:window-line-height
           #:move-window
           #:resize-window
           #:raise-window
           #:lower-window))
(in-package :vico-lib.ui)

(defclass ui ()
  ((ui-thread :initarg :ui-thread
              :reader ui-thread
              :type concurrency:thread))
  (:documentation "To be subclassed by all user frontends."))

(defmacro define-protocol (name (&rest arglist) &optional documentation)
  `(defgeneric ,name (,@arglist)
     (:method (,@arglist)
       (declare (ignorable ,@(loop :for arg in arglist
                                   :when (unless (or (eq arg '&rest)
                                                     (eq arg '&optional)
                                                     (eq arg '&key)
                                                     (eq arg '&allow-other-keys))
                                           arg)
                                     :collect it)))
       (error 'conditions:vico-protocol-unimplemented-error :ui-type ',(first arglist)
                                                           :ui-object ,(first arglist)))
     ,(list :documentation (or documentation "undocumented"))))

(define-protocol windows (ui) "Returns a list of windows under the ui instance UI.")
(define-protocol (setf windows) (new-value ui))
