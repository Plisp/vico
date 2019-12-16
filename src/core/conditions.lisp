;;;; This file contains the definition of all conditions

(defpackage :vico-core.conditions
  (:use :cl)
  (:export #:vico-condition
           #:vico-error
           ))
(in-package :vico-core.conditions)

(define-condition vico-condition (simple-condition)
  ()
  (:documentation "All conditions signaled by vico are of this type."))

(define-condition vico-error (simple-error)
  ()
  (:documentation "All errors signaled by vico are of this type."))
