;;;; buffer implementation

(defpackage :vico-core.buffer
  (:use :cl)
  (:local-nicknames (:conditions :vico-core.conditions))
  (:shadow :length :char :subseq)
  (:export #:buffer
           #:length
           #:char
           #:subseq
           #:line-count
           #:line-number-offset
           #:offset-in-bytes
           #:insert
           #:erase
           #:undo #:redo))
(in-package :vico-core.buffer)

(defclass buffer ()
  ()
  (:documentation "Base class for buffers."))

(defvar *max-buffer-size* (expt 2 50)
  "For type declaration purposes (for in-memory data structures). No buffer should hold
more than a pebibyte in memory. Other methods such as on demand paging are recommended.")

;; readers

(defgeneric length (buffer)
  (:method ((buffer sequence))
    (cl:length buffer)))

(defgeneric char (buffer n)
  (:method ((buffer string) n)
    (cl:char buffer n)))

(defgeneric subseq (buffer start &optional end)
  (:method ((buffer sequence) start &optional end)
    (cl:subseq buffer start end)))

(defgeneric line-count (buffer)
  (:documentation
   "Returns the number of lines in BUFFER."))

(defgeneric line-number-offset (buffer line-number)
  (:documentation
   "Returns the offset of the first character in the LINE-NUMBERth line in BUFFER, (non
-linefeed character). If (1+ (line-count buffer)) is passed as the second argument, the
'off-end' offset (size of the buffer) will be returned."))

(defgeneric offset-in-bytes (buffer offset)
  (:method ((buffer buffer) offset)
    (babel:string-size-in-octets (subseq buffer 0 offset)))
  (:documentation
   "Returns OFFSET into BUFFER in terms of bytes."))

;; writers

(defgeneric insert (buffer string offset)
  (:documentation
   "Inserts STRING into BUFFER at OFFSET. Returns the inserted string."))

(defgeneric erase (buffer start &optional end)
  (:documentation
   "Erases the characters in BUFFER between the offsets START and END (defaults to
(1+ start)). Returns the deleted string."))

(defgeneric undo (buffer)
  (:documentation
   "Undo the most recent edit to BUFFER. Returns BUFFER."))

(defgeneric redo (buffer)
  (:documentation
   "Redo the most recent edit to BUFFER. Returns BUFFER."))
