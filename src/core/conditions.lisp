(defpackage :vico-core.conditions
  (:use :cl)
  (:export #:vico-condition
           #:vico-error
           #:vico-syscall-error
           ))
(in-package :vico-core.conditions)

(define-condition vico-condition (simple-condition)
  ()
  (:documentation "All conditions signaled by vico are of this type."))

(define-condition vico-error (simple-error)
  ()
  (:documentation "All errors signaled by vico internals are of this type."))

(define-condition vico-syscall-error (vico-error)
  ()
  (:documentation "Syscall failure TODO have a slot for strerror()"))
