;;;; This file contains the definition of all conditions

(defpackage :vico-core.conditions
  (:use :cl)
  (:export
   :vico-condition
   :vico-error
   :vico-protocol-unimplemented-error
   ))
(in-package :vico-core.conditions)

(define-condition vico-condition (simple-condition)
  ()
  (:documentation "All conditions signaled by vico are of this type."))

(define-condition vico-error (simple-error)
  ()
  (:documentation "All errors signaled by vico are of this type."))

(define-condition vico-protocol-unimplemented-error (vico-error)
  ((ui-type :initarg :type
            :reader ui-type)
   (ui-object :initarg :object
              :reader ui-object))
  (:report (lambda (condition stream)
             (format stream "The object ~A does not implement this ~A protocol."
                     (ui-object condition)
                     (ui-type condition)))))
