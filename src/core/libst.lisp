(defpackage :vico-core.buffer.slice-table
  (:use :cl :alexandria)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:io :vico-core.io)
                    (:enc :babel-encodings)
                    (:ffi :cffi)))
(in-package :vico-core.buffer.slice-table)

(ffi:defctype size-t
  #+64-bit :uint64
  #+32-bit :uint32)

(ffi:load-foreign-library
 (asdf:system-relative-pathname :vico-core "src/core/libst.so"))

(defclass slice-table-buffer (buf:buffer)
  ((slice-table :initarg :slice-table
                :type ffi:foreign-pointer)))

(defun %make-buffer-with-contents (string)
  (let ((bufptr (ffi:foreign-funcall "st_new" :pointer)))
    (ffi:with-foreign-string ((s size) string :null-terminated-p nil)
      (ffi:foreign-funcall "st_insert"
                           :pointer bufptr
                           size-t 0
                           :pointer s
                           size-t size
                           :void))
    bufptr))

(defmethod buf:make-buffer ((type (eql :slice-table)) &key initial-contents initial-stream)
  (let ((buffer (make-instance 'slice-table-buffer)))
    (cond (initial-stream
           (if (io:file-stream-p initial-stream)
               (setf (slot-value buffer 'slice-table)
                     (ffi:foreign-funcall "st_new_from_file"
                                          :string (pathname initial-stream)
                                          :pointer))
               (setf (slot-value buffer 'slice-table)
                     (%make-buffer-with-contents (read-stream-content-into-string
                                                  initial-stream)))))
          (initial-contents
           (setf (slot-value buffer 'slice-table)
                 (%make-buffer-with-contents initial-contents)))
          (t (setf (slot-value buffer 'slice-table)
                   (ffi:foreign-funcall "st_new" :pointer))))
    buffer))

(defmethod buf:close-buffer ((buffer slice-table-buffer))
  (let ((ptr (slot-value buffer 'slice-table)))
    (tg:finalize buffer
                 (lambda ()
                   (ffi:foreign-funcall "st_free" :pointer ptr :void)))))

;;(defmethod buf:copy-buffer )

()

;; TODO BOUNDS CHECKS on edits

()

()

;; cursors

;; TODO do we have to worry about GC pressure due to foreign cursor allocations?
;;      they're 336 bytes atm on x64, and must be freed using finalizers

(defstruct cursor
  ;; cursors must keep buffers alive for the GC
  (buffer nil :type (or null slice-table-buffer))
  (%ptr (ffi:null-pointer) :type ffi:foreign-pointer))

(defmethod buf:make-cursor ((buffer slice-table-buffer) index &key track static)
  (declare (ignore track static))
  (let ((%st (slot-value buffer 'slice-table))
        (cursor (make-cursor :buffer buffer)))
    (setf (cursor-ptr cursor)
          (ffi:foreign-funcall "st_iter_new"
                               :pointer %st
                               size-t index
                               :pointer))
    cursor))

(defmethod buf:byte-at ((cursor cursor))
  (ffi:foreign-funcall "st_iter_byte"
                       :pointer (cursor-ptr cursor)
                       :char))

(defmethod buf:cursor-next ((cursor cursor) &optional (count 1))
  (ffi:foreign-funcall "st_iter_next_byte"
                       :pointer (cursor-ptr cursor)
                       size-t count
                       :char))

(defmethod buf:cursor-prev ((cursor cursor) &optional (count 1))
  (ffi:foreign-funcall "st_iter_prev_byte"
                       :pointer (cursor-ptr cursor)
                       size-t count
                       :char))




;;; for the repl

;; (ffi:with-foreign-object (c 'size-t)
;;   (ffi:foreign-string-to-lisp (st-iter-chunk it c)
;;                               :count (ffi:mem-ref c :long)))

(ffi:defcfun "st_new" :pointer)
(ffi:defcfun "st_new_from_file" :pointer
  (path :string))
(ffi:defcfun "st_free" :void (st :pointer))
(ffi:defcfun "st_clone" :void (st :pointer))

(ffi:defcfun "st_pprint" :void (st :pointer))
(ffi:defcfun "st_size" size-t (st :pointer))

(ffi:defcfun "st_insert" size-t ; return lfs
  (st :pointer)
  (pos size-t)
  (data :pointer)
  (datalen size-t))
(ffi:defcfun "st_delete" size-t
  (st :pointer)
  (pos size-t)
  (len size-t))

(ffi:defcfun "st_iter_new" :pointer
  (st :pointer)
  (pos size-t))
(ffi:defcfun "st_iter_free" :void
  (it :pointer))
(ffi:defcfun "st_iter_pos" size-t
  (it :pointer))
(ffi:defcfun "st_iter_byte" :char
  (it :pointer))
(ffi:defcfun "st_iter_to" :pointer
  (it :pointer)
  (pos size-t))
(ffi:defcfun "st_iter_next_byte" :char
  (it :pointer)
  (count size-t))
(ffi:defcfun "st_iter_chunk" :pointer
  (it :pointer)
  (len :pointer))
(ffi:defcfun "st_iter_next_chunk" :bool
  (it :pointer))
(ffi:defcfun "st_iter_prev_chunk" :bool
  (it :pointer))
