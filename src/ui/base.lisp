;;;; low level frontend protocol

(in-package :vico-lib.ui)

(defvar *frontends* (list)
  "UI instances should add themselves to this list on startup.")

(defclass ui () ()
  (:documentation "To be subclassed by all frontends."))

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
       (error 'condition:vico-protocol-unimplemented-error
              :ui-type ',(first arglist)
              :ui-object ,(first arglist)))
     ,(list :documentation (or documentation "undocumented"))))

(define-protocol windows (ui) "Returns a list of windows under the frontend UI.")
(define-protocol (setf list-windows) (new-value))
