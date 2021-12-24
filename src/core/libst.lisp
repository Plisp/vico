(defpackage #:vico-core.buffer.slice-table
  (:use :cl :alexandria)
  (:shadow #:close #:delete)
  (:local-nicknames (#:ffi #:cffi)))
(in-package #:vico-core.buffer.slice-table)

;;; ffi

(deftype idx () '(unsigned-byte #+64-bit 62 #+32-bit 32))

(ffi:load-foreign-library
 (asdf:system-relative-pathname :vico-core "src/core/libst.so"))

(ffi:defcfun ("st_new" %st-new) :pointer)
(ffi:defcfun ("st_new_from_file" %st-new-from-file) :pointer
  (path :string))

(ffi:defcfun ("st_free" %st-free) :void (st :pointer))
(ffi:defcfun ("st_clone" %st-clone) :pointer (st :pointer))

(declaim (ftype (function (ffi:foreign-pointer) idx) %st-size)
         (inline %st-size %st-insert %st-delete))
(ffi:defcfun ("st_size" %st-size) :size (st :pointer))

(ffi:defcfun ("st_insert" %st-insert) :size ; return lfs
  (st :pointer)
  (pos :size)
  (data :pointer)
  (datalen :size))

(ffi:defcfun ("st_delete" %st-delete) :size
  (st :pointer)
  (pos :size)
  (len :size))

;; iter

(ffi:defcfun ("st_iter_init" %iter-init) :pointer
  (it :pointer)
  (st :pointer)
  (index :size))
(ffi:defcfun ("st_iter_new" %iter-new) :pointer
  (st :pointer)
  (index :size))
(ffi:defcfun ("st_iter_free" %iter-free) :void
  (it :pointer))

(declaim (ftype (function (ffi:foreign-pointer) idx) %iter-pos)
         (inline %iter-pos %iter-to))
(ffi:defcfun ("st_iter_pos" %iter-pos) :size
  (it :pointer))
(ffi:defcfun ("st_iter_to" %iter-to) :pointer
  (it :pointer)
  (pos :size))

(declaim (inline %iter-byte %iter-next-byte %iter-prev-byte))
(ffi:defcfun ("st_iter_byte" %iter-byte) :int
  (it :pointer))

(ffi:defcfun ("st_iter_next_byte" %iter-next-byte) :int
  (it :pointer)
  (count :size))

(ffi:defcfun ("st_iter_prev_byte" %iter-prev-byte) :int
  (it :pointer)
  (count :size))

(declaim (inline %iter-char %iter-next-char %iter-prev-char))
(ffi:defcfun ("st_iter_cp" %iter-char) :long
  (it :pointer))

(ffi:defcfun ("st_iter_next_cp" %iter-next-char) :long
  (it :pointer)
  (count :size))

(ffi:defcfun ("st_iter_prev_cp" %iter-prev-char) :long
  (it :pointer)
  (count :size))

(declaim (inline %iter-next-line %iter-prev-line))
(ffi:defcfun ("st_iter_next_line" %iter-next-line) :bool
  (it :pointer)
  (count :size))

(ffi:defcfun ("st_iter_prev_line" %iter-prev-line) :bool
  (it :pointer)
  (count :size))

;; Do not expose these. Use them only if you know what you're doing
(declaim (inline %iter-chunk %iter-next-chunk %iter-prev-chunk))
(ffi:defcfun ("st_iter_chunk" %iter-chunk) :pointer
  (it :pointer)
  (len :pointer))

(ffi:defcfun ("st_iter_next_chunk" %iter-next-chunk) :bool
  (it :pointer))
(ffi:defcfun ("st_iter_prev_chunk" %iter-prev-chunk) :bool
  (it :pointer))

;;; impl

;; This a struct since it can be. This is not ever meant to be subclassed
(defstruct (st (:constructor %make-st))
  (ptr (ffi:null-pointer) :type ffi:foreign-pointer)
  (size 0 :type idx) ; requires calculation from C. may as well cache it
  ;; When owner is null, clone before every edit operation.
  (owner nil :type (or null bt:thread)))

(defun make-buffer-with-contents (string)
  (let ((bufptr (%st-new)))
    (ffi:with-foreign-string ((%string size) string :null-terminated-p nil)
      (%st-insert bufptr 0 %string size))
    bufptr))

(defun make-st (&key initial-contents initial-stream)
  (let ((st (%make-st :owner nil))
        (ptr (cond (initial-stream
                    (if (typep initial-stream 'file-stream)
                        (%st-new-from-file (namestring (truename initial-stream)))
                        (make-buffer-with-contents
                         (read-stream-content-into-string initial-stream))))
                   (initial-contents
                    (make-buffer-with-contents initial-contents))
                   (t
                    (%st-new)))))
    (tg:finalize st (lambda () (%st-free ptr))) ; may have multiple refs in same thread
    (setf (st-ptr st) ptr
          (st-size st) (%st-size (st-ptr st)))
    st))

(declaim (inline size))
(defun size (buffer)
  (declare (type st buffer))
  (st-size buffer))

(declaim (ftype (function (st) st) copy)
         (inline copy))
(defun copy (buffer)
  "Returns a thread safe copy of BUFFER."
  (declare (type st buffer))
  (let ((st (%make-st :owner nil
                      :ptr (%st-clone (st-ptr buffer))
                      :size (st-size buffer))))
    (tg:finalize st (lambda () (%st-free (st-ptr st))))
    st))

(defun transient (buffer)
  (declare (type st buffer))
  (setf (st-owner buffer) (bt:current-thread))
  buffer)

(defun persistent (buffer)
  (declare (type st buffer))
  (setf (st-owner buffer) nil)
  buffer)

(declaim (ftype (function (st idx string) (or null st)) insert)
         (inline insert))
(defun insert (buffer index string)
  "Inserts STRING at INDEX in BUFFER. Returns the byte length of STRING - NOT the LENGTH."
  (declare (optimize speed))
  (when (<= index (st-size buffer))
    ;; add stack-allocated fast path using with-foreign-pointer if needed
    (ffi:with-foreign-string ((%string len) string :null-terminated-p nil)
      ;; update in-place only if transient
      (let ((buffer (if (eq (st-owner buffer) (bt:current-thread))
                        buffer
                        (copy buffer))))
        (%st-insert (st-ptr buffer) index %string len)
        (incf (st-size buffer) (the idx len))
        buffer))))

(declaim (ftype (function (st idx idx) (or null st)) delete)
         (inline delete))
(defun delete (buffer index n)
  "Deletes N bytes at INDEX in BUFFER. Returns no useful value"
  (declare (optimize speed))
  (when (<= (+ index n) (st-size buffer))
    (let ((buffer (if (eq (st-owner buffer) (bt:current-thread))
                      buffer
                      (copy buffer))))
      (%st-delete (st-ptr buffer) index n)
      (decf (st-size buffer) n)
      buffer)))

;;; cursors

;; TODO do we have to worry about memory pressure due to foreign cursor allocations?
;;      if this becomes an issue, switch the implementation to the lisp side

(declaim (inline %make-cursor)) ; may stack allocate on sbcl
(defstruct (cursor (:constructor %make-cursor)
                   (:copier nil))
  ;; cursors must keep buffers alive for the GC
  (st (error "cursor is not attached to a st buffer!") :type st)
  (ptr (ffi:null-pointer) :type ffi:foreign-pointer))

(defun make-cursor (buffer index)
  (declare (type st buffer))
  (let ((%iter (%iter-new (st-ptr buffer) index))
        (cursor (%make-cursor :st buffer)))
    (setf (cursor-ptr cursor) %iter)
    (tg:finalize cursor (lambda () (%iter-free %iter)))
    cursor))

(declaim (inline cursor-to))
(defun cursor-to (cursor index)
  (%iter-to (cursor-ptr cursor) index))

(declaim (ftype (function (cursor) idx) index-at)
         (inline index-at))
(defun index-at (cursor)
  (declare (optimize speed))
  (%iter-pos (cursor-ptr cursor)))

;;; relational ops
(macrolet ((def (op)
             `(defun ,(symbolicate 'cursor op) (cursor1 cursor2)
                (,op (index-at cursor1) (index-at cursor2)))))
  (def -)
  (def =) (def /=)
  (def >) (def >=)
  (def <) (def <=))

(declaim (ftype (function (cursor) (or null (unsigned-byte 8))) byte-at)
         (inline byte-at))
(defun byte-at (cursor)
  "Returns the byte at CURSOR, or NIL if it was at the end of the document."
  (declare (optimize speed))
  (let ((c (%iter-byte (cursor-ptr cursor))))
    (unless (minusp c)
      c)))

(defun chunk-at (cursor)
  (ffi:with-foreign-object (c ':size)
    (values (ffi:foreign-string-to-lisp (%iter-chunk (cursor-ptr cursor) c)
                                        :count (ffi:mem-ref c :long)))))

(declaim (inline cursor-next cursor-prev))
(defun cursor-next (cursor &optional (count 1))
  "Moves CURSOR forward COUNT bytes and returns the byte there, or NIL if the end
was reached."
  (declare (optimize speed)
           (type idx count))
  (when (<= (+ count (index-at cursor))
            (st-size (cursor-st cursor)))
    (%iter-next-byte (cursor-ptr cursor) count)))

(defun cursor-prev (cursor &optional (count 1))
  "Moves CURSOR backward COUNT bytes and returns the byte there, or NIL if the start
was reached."
  (declare (optimize speed)
           (type idx count))
  (when (<= count (%iter-pos (cursor-ptr cursor)))
    (%iter-prev-byte (cursor-ptr cursor) count)))

(defun copy-cursor (cursor)
  (declare (type cursor cursor))
  (make-cursor (cursor-st cursor) (index-at cursor)))

(declaim (inline char-at cursor-next-char cursor-prev-char))
(defun char-at (cursor)
  (declare (optimize speed))
  (let ((cp (%iter-char (cursor-ptr cursor))))
    (unless (minusp cp)
      (code-char cp))))

(defun cursor-next-char (cursor &optional (count 1))
  (declare (optimize speed))
  (let ((cp (%iter-next-char (cursor-ptr cursor) count)))
    (unless (minusp cp)
      (code-char cp))))

(defun cursor-prev-char (cursor &optional (count 1))
  (declare (optimize speed))
  (let ((cp (%iter-prev-char (cursor-ptr cursor) count)))
    (unless (minusp cp)
      (code-char cp))))

;; It does not make sense to return a count of codepoints, as the endpoint is likely known
;; in advance only in terms of bytes
(defun subseq-at (cursor length)
  "Returns a string from the LENGTH bytes following CURSOR."
  (let ((copy (copy-cursor cursor))
        (buffer (make-array length :element-type 'character)))
    (loop :for i :from 0
          :until (>= (cursor- copy cursor) length)
          :for c = (char-at copy)
          :do (setf (aref buffer i) (if (null c) #\REPLACEMENT_CHARACTER c))
              (cursor-next-char copy)
          :finally (return buffer))))

(declaim (inline cursor-next-line cursor-prev-line))
(defun cursor-next-line (cursor &optional (count 1))
  "Returns NIL if not enough lines remaining."
  (declare (optimize speed))
  (%iter-next-line (cursor-ptr cursor) count))

(defun cursor-prev-line (cursor &optional (count 1))
  "Returns NIL if not enough lines remaining."
  (declare (optimize speed))
  (%iter-prev-line (cursor-ptr cursor) count))

(defun cursor+ (cursor n)
  (make-cursor (cursor-st cursor) (+ (index-at cursor) n)))
