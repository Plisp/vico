(defpackage #:vico-core.buffer
  (:local-nicknames (#:st #:vico-core.buffer.slice-table))
  (:use :cl)
  (:export #:buffer
           #:cursors

           #:cursor
           #:copy-cursor))
(in-package #:vico-core.buffer)

(deftype cursor () 'st::cursor)

(defclass buffer ()
  ((active :initarg :active ; this one's transient
           :type st::st)
   (history :initform (list)
            :reader history)
   (cursors :initarg :cursors
            :accessor cursors)))

(defmethod cursor ((buffer buffer) index)
  (st::make-cursor (slot-value buffer 'active) index))

(declaim (inline copy-cursor))
(defun copy-cursor (cursor)
  (st::copy-cursor cursor))

;; XXX doesn't belong here - move to commands
;; these always operate on the active slicetable - only the main thread is allowed to edit

(defmethod insert-at ((buffer buffer) insert-cursor string)
  (loop :with old-size = (st::st-size buffer)
        :with insert-idx = (st::index-at insert-cursor)
        :with bytelen = (- (st::st-size (st::insert buffer insert-idx string))
                           old-size)
        :with cursors = (cursors buffer)
        :for i :from 0
        :for c :across cursors
        :do (when (>= (st::index-at c) insert-idx)
              (st::cursor-to c (+ (st::index-at c) bytelen)))))

(defmethod delete-at ((buffer buffer) cursor n)
  (loop :with start-idx = (st::index-at cursor)
        :initially (st::delete buffer start-idx n)
        :with cursors = (cursors buffer)
        :for i :from 0
        :for c :across cursors
        :do (cond ((> (st::index-at c) (+ start-idx n))
                   (st::cursor-to c (- (st::index-at c) n)))
                  ((> (st::index-at c) start-idx)
                   (st::cursor-to c start-idx)))))
