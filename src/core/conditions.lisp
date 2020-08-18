(defpackage :vico-core.conditions
  (:use :cl)
  (:export #:vico-condition
           #:vico-error
           #:vico-syscall-error
           #:vico-cursor-invalid
           #:vico-buffer-bounds-error
           #:buffer-bounds-error-buffer
           #:buffer-bounds-error-bounds
           #:buffer-bounds-error-index
           #:buffer-bounds-error-line-number
           #:vico-bad-index
           #:vico-bad-line-number
           ))
(in-package :vico-core.conditions)

(define-condition vico-condition (simple-condition)
  ()
  (:documentation "All vico-specific conditions should subclass this type."))

(define-condition vico-error (simple-error)
  ()
  (:documentation "All errors signaled by vico internals should subclass this type."))

;;; buffer

(define-condition vico-cursor-invalid (vico-error)
  ((cursor :initarg :cursor
           :reader cursor-invalid-error-cursor))
  (:report (lambda (condition stream)
             (format stream "invalidated cursor ~a"
                     (cursor-invalid-error-cursor condition)))))

(define-condition vico-buffer-bounds-error (vico-error)
  ((buffer :initarg :buffer
           :reader buffer-bounds-error-buffer
           :type piece-table)
   (bounds :initform nil
           :initarg :bounds
           :reader buffer-bounds-error-bounds))
  (:documentation "Signaled when trying to access out of bounds."))

(define-condition vico-bad-index (vico-buffer-bounds-error)
  ((index :initarg :index
          :reader buffer-bounds-error-index))
  (:report (lambda (condition stream)
             (format stream "index ~d is out of bounds for ~a. ~
                             Should be a valid integer index within [~d:~d]."
                     (buffer-bounds-error-index condition)
                     (buffer-bounds-error-buffer condition)
                     (car (buffer-bounds-error-bounds condition))
                     (cdr (buffer-bounds-error-bounds condition))))))

(define-condition vico-bad-line-number (vico-buffer-bounds-error)
  ((line-number :initform nil
                :initarg :line-number
                :reader buffer-bounds-error-line-number))
  (:report (lambda (condition stream)
             (format stream "line-number ~d is out of bounds for ~a. ~
                             Should be an integer within [~d:~d]."
                     (buffer-bounds-error-line-number condition)
                     (buffer-bounds-error-buffer condition)
                     (car (buffer-bounds-error-bounds condition))
                     (cdr (buffer-bounds-error-bounds condition))))))

;;; misc

(define-condition vico-syscall-error (vico-error)
  ()
  (:documentation "Syscall failure TODO have a slot for strerror()"))
