(defpackage :vico-core.buffer.slice-table
  (:use :cl :alexandria)
  (:shadow :close :delete)
  (:local-nicknames (:io :vico-core.io)
                    (:ffi :cffi)))
(in-package :vico-core.buffer.slice-table)

;;; ffi

(deftype idx () '(unsigned-byte #+64-bit 62 #+32-bit 32))

(ffi:defctype size-t
  #+64-bit :uint64
  #+32-bit :uint32)

(ffi:load-foreign-library
 (asdf:system-relative-pathname :vico-core "src/core/libst.so"))

(ffi:defcfun ("st_new" %st-new) :pointer)
(ffi:defcfun ("st_new_from_file" %st-new-from-file) :pointer
  (path :string))

(ffi:defcfun ("st_free" %st-free) :void (st :pointer))
(ffi:defcfun ("st_clone" %st-clone) :void (st :pointer))

(declaim (ftype (function (ffi:foreign-pointer) idx) %st-size)
         (inline %st-size %st-insert %st-delete))
(ffi:defcfun ("st_size" %st-size) size-t (st :pointer))

(ffi:defcfun ("st_insert" %st-insert) size-t ; return lfs
  (st :pointer)
  (pos size-t)
  (data :pointer)
  (datalen size-t))

(ffi:defcfun ("st_delete" %st-delete) size-t
  (st :pointer)
  (pos size-t)
  (len size-t))

;; iter

(ffi:defcfun ("st_iter_new" %iter-new) :pointer
  (st :pointer))
(ffi:defcfun ("st_iter_free" %iter-free) :void
  (it :pointer))

(declaim (ftype (function (ffi:foreign-pointer) idx) %iter-pos)
         (inline %iter-pos %iter-to))
(ffi:defcfun ("st_iter_pos" %iter-pos) size-t
  (it :pointer))
(ffi:defcfun ("st_iter_to" %iter-to) :pointer
  (it :pointer)
  (pos size-t))

(declaim (inline %iter-byte %iter-next-byte %iter-prev-byte))
(ffi:defcfun ("st_iter_byte" %iter-byte) :char
  (it :pointer))

(ffi:defcfun ("st_iter_next_byte" %iter-next-byte) :char
  (it :pointer)
  (count size-t))

(ffi:defcfun ("st_iter_prev_byte" %iter-prev-byte) :char
  (it :pointer)
  (count size-t))

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
(defstruct st
  (ptr (ffi:null-pointer) :type ffi:foreign-pointer)
  (size 0 :type idx) ; requires calculation from C. may as well cache it
  ;; When owner is null, clone before every edit operation.
  (owner nil :type (or null bt:thread)))

(defun make-buffer-with-contents (string)
  (let ((bufptr (%st-new)))
    (ffi:with-foreign-string ((%string size) string :null-terminated-p nil)
      (%st-insert bufptr 0 %string size))
    bufptr))

(defun make-buffer (&key initial-contents initial-stream)
  (when (and initial-contents initial-stream)
    (error "only one of INITIAL-CONTENTS and INITIAL-STREAM may be provided"))
  (let ((st (make-st :owner nil)))
    (setf (st-ptr st) (cond
                        (initial-stream
                         (if (io:file-stream-p initial-stream)
                             (%st-new-from-file (pathname initial-stream))
                             (make-buffer-with-contents
                              (read-stream-content-into-string initial-stream))))
                        (initial-contents
                         (make-buffer-with-contents initial-contents))
                        (t
                         (%st-new)))
          (st-size st) (%st-size (st-ptr st)))
    st))

;; TODO ignore if attempting double-free here and with iterators
(defun close (buffer)
  (let ((%st (st-ptr buffer)))
    (tg:finalize buffer (lambda () (%st-free %st)))))

(declaim (inline size))
(defun size (buffer)
  (declare (type st buffer))
  (st-size buffer))

(defun transient (buffer)
  (declare (type st buffer))
  (setf (st-owner buffer) (bt:current-thread)))

(declaim (ftype (function (st string idx) (or null idx)) insert)
         (inline insert))
(defun insert (buffer string index)
  (declare (optimize speed))
  (when (<= index (st-size buffer))
    ;; add stack-allocated fast path using with-foreign-pointer if needed
    (ffi:with-foreign-string ((%string len) string :null-terminated-p nil)
      (%st-insert (st-ptr buffer) index %string len)
      (incf (st-size buffer) (the idx len)))))

(declaim (ftype (function (st idx idx) (or null idx)) delete)
         (inline delete))
(defun delete (buffer index n)
  "Deletes N bytes at INDEX in BUFFER"
  (declare (optimize speed))
  (when (<= (+ index n) (st-size buffer))
    (%st-delete (st-ptr buffer) index n)
    (decf (st-size buffer) n)))

;;; cursors

;; TODO do we have to worry about memory pressure due to foreign cursor allocations?
;;      if this becomes an issue, switch the implementation to the lisp-side


(defstruct (cursor (:constructor %make-cursor)
                   (:copier nil))
  ;; cursors must keep buffers alive for the GC
  (st (error "cursor is not attached to a st buffer!") :type st)
  (ptr (ffi:null-pointer) :type ffi:foreign-pointer))

;; XXX unsafe for dumping, as addresses will not be preserved
(defun make-cursor (buffer index)
  (declare (type st buffer))
  (declare (ignore index))
  (let ((%iter (%iter-new (st-ptr buffer)))
        (cursor (%make-cursor :st buffer)))
    (setf (cursor-ptr cursor) %iter)
    (tg:finalize buffer (lambda () (%iter-free %iter)))
    cursor))

(declaim (ftype (function (cursor) idx) index-at)
         (inline index-at))
(defun index-at (cursor)
  (declare (optimize speed))
  (%iter-pos (cursor-ptr cursor)))

(declaim (ftype (function (cursor) (or null (unsigned-byte 8))) byte-at)
         (inline byte-at))
(defun byte-at (cursor)
  (declare (optimize speed))
  (let ((b (%iter-byte (cursor-ptr cursor))))
    (if (plusp b) ; -1 means end-of-file
        b
        nil)))

(defun chunk-at (cursor)
  (ffi:with-foreign-object (c 'size-t)
    (values (ffi:foreign-string-to-lisp (%iter-chunk (cursor-ptr cursor) c)
                                        :count (print (ffi:mem-ref c :long))))))

(declaim (inline cursor-next cursor-prev))
(defun cursor-next (cursor &optional (count 1))
  (declare (optimize speed)
           (type idx count))
  (when (<= (+ count (index-at cursor))
            (st-size (cursor-st cursor)))
    (%iter-next-byte (cursor-ptr cursor) count)))

(defun cursor-prev (cursor &optional (count 1))
  (declare (optimize speed)
           (type idx count))
  (when (<= count (%iter-pos (cursor-ptr cursor)))
    (%iter-prev-byte (cursor-ptr cursor) count)))

(defun copy-cursor (cursor)
  (declare (type cursor cursor))
  (make-cursor (cursor-st cursor) (index-at cursor)))

(declaim (notinline char-at cursor-next-char cursor-prev-char))
;; (defun char-at (cursor)
;;   (declare (optimize speed))
;;   (%iter-char (cursor-ptr cursor)))

;; (defun cursor-next-char (cursor &optional count)
;;   (declare (optimize speed))
;;   )

;; (defun cursor-prev-char (cursor &optional count)
;;   (declare (optimize speed))
;;   )

;; (defun subseq-at (cursor length)
;;   "Returns a string of LENGTH *codepoints*."
;;   (let ((copy (copy-cursor cursor))
;;         (buffer (make-array length :element-type 'character)))
;;     (loop :for i :below length
;;           :do (setf (aref buffer i) (char-at copy))
;;               (cursor-next-char copy)
;;           :finally (return buffer))))

(declaim (notinline cursor-next-line cursor-prev-line))
