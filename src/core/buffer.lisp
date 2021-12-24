(defpackage #:vico-core.buffer
  (:local-nicknames (#:st #:vico-core.buffer.slice-table))
  (:use :cl)
  (:import-from #:vico-core.buffer.slice-table
                #:st #:make-st #:st-size
                #:make-cursor
                ;; KEEP IN SYNC vvv
                #:cursor #:copy-cursor
                #:cursor-to
                #:cursor-next #:cursor-prev
                #:cursor-next-char #:cursor-prev-char
                #:cursor-next-line #:cursor-prev-line
                #:index-at #:byte-at #:char-at #:subseq-at

                #:cursor+ #:cursor-
                #:cursor= #:cursor/=
                #:cursor< #:cursor<= #:cursor> #:cursor>=
                )
  (:export #:buffer #:bufferp
           #:cursors
           #:active #:history
           #:size

           #:insert-at #:delete-at

           ;; KEEP IN SYNC vvv
           #:cursor #:copy-cursor
           #:with-cursor #:with-cursors*
           #:cursor-to
           #:cursor-next #:cursor-prev
           #:cursor-next-char #:cursor-prev-char
           #:cursor-next-line #:cursor-prev-line
           #:index-at #:byte-at #:char-at #:subseq-at

           #:cursor+ #:cursor-
           #:cursor= #:cursor/=
           #:cursor< #:cursor<= #:cursor> #:cursor>=
           ;; #:cursor-line+ #:cursor-line-
           ;; #:cursor-line=
           ;; #:cursor-line< #:cursor-line<= #:cursor-line> #:cursor-line>=
           ))
(in-package #:vico-core.buffer)

(defclass buffer ()
  ((active :initarg :active ; this one's transient
           :reader active
           :type st)
   (history :initform (list)
            :reader history)
   (cursors :initarg :cursors
            :accessor cursors)))

(defun bufferp (object)
  (typep object 'buffer))

(defun size (buffer)
  (st-size (active buffer)))

(defun buffer (&optional from)
  (let ((%buffer (etypecase from
                   (null (make-st))
                   (stream (make-st :initial-stream from))
                   (string (make-st :initial-contents from))
                   (pathname (with-open-file (s from)
                               (make-st :initial-stream s))))))
    (make-instance 'buffer :active %buffer
                           :cursors (make-array 2))))

(defmethod cursor ((buffer buffer) index)
  (make-cursor (slot-value buffer 'active) index))

;; wow this is ugly
(defmacro with-cursor ((var buffer-or-cursor &optional (index 0)) &body body)
  "Use whenever possible. WARNING: the new cursor is deallocated at the end of the scope."
  (alexandria:with-gensyms (%iter %st)
    `(etypecase ,buffer-or-cursor
       (cursor (let ((,var (copy-cursor ,buffer-or-cursor)))
                 ,@body))
       (buffer
        (cffi:with-foreign-pointer (,%iter (cffi:foreign-funcall "st_iter_size" :size))
          (let* ((,%st (active ,buffer-or-cursor))
                 (,var (st::%make-cursor :st ,%st)))
            (declare (dynamic-extent ,var))
            (st::%iter-init ,%iter (st::st-ptr ,%st) ,index)
            (setf (st::cursor-ptr ,var) ,%iter)
            ;; no finalization needed - deallocated on scope exit
            ,@body))))))

(defmacro with-cursors* (bindings &body body)
  (if bindings
      `(with-cursor ,(car bindings)
         (with-cursors* ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;; XXX doesn't belong here - move to commands
;; these always operate on the active slicetable - only the main thread is allowed to edit

(defmethod insert-at ((buffer buffer) insert-cursor string)
  (loop :with old-size = (size buffer)
        :with insert-idx = (index-at insert-cursor)
        :with bytelen = (- (size (st::insert buffer insert-idx string))
                           old-size)
        :with cursors = (cursors buffer)
        :for i :from 0
        :for c :across cursors
        :do (when (>= (index-at c) insert-idx)
              (cursor-to c (+ (index-at c) bytelen)))))

(defmethod delete-at ((buffer buffer) cursor n)
  (loop :with start-idx = (index-at cursor)
        :initially (st::delete buffer start-idx n)
        :with cursors = (cursors buffer)
        :for i :from 0
        :for c :across cursors
        :do (cond ((> (index-at c) (+ start-idx n))
                   (cursor-to c (- (index-at c) n)))
                  ((> (index-at c) start-idx)
                   (cursor-to c start-idx)))))
