(defpackage :vico-core.buffer
  (:use :cl)
  (:local-nicknames (:conditions :vico-core.conditions))
  (:shadow :char :length :subseq)
  (:export #:buffer
           #:length
           #:char
           #:subseq
           #:line-count
           #:line-number-offset
           #:offset-in-bytes
           #:byte-length
           #:insert
           #:erase
           #:undo #:redo
           #:write-to-file
           #:keybinds))
(in-package :vico-core.buffer)

(defclass buffer ()
  ()
  (:documentation
   "Base class for buffers. In a general sense, buffers are anything that represent an
ordered sequence of CHARACTERs.
All buffers should define a method on INITIALIZE-INSTANCE accepting the keyword parameter
:INITIAL-CONTENTS for specifying the contents of the buffer (in an unspecified format).
:INITIAL-FILE may also be optionally accepted, indicating that the provided pathname's
contents should be used to initialize the buffer. This overrides INITIAL-CONTENTS.
If any offset passed is out of bounds, these functions should signal a condition of type
VICO-BOUNDS-ERROR."))

(defvar *max-buffer-size* (expt 2 50)
  "For type declaration purposes (for in-memory data structures). No buffer should hold
more than a pebibyte (assuming bytes) in memory.")

;; readers

(defgeneric length (buffer)
  (:method ((seq sequence))
    (cl:length seq))
  (:documentation
   "Returns the length of BUFFER in characters."))

(defgeneric char (buffer n)
  (:method ((seq string) n)
    (cl:char seq n)))

(defgeneric subseq (buffer start &optional end)
  (:method ((seq sequence) start &optional end)
    (cl:subseq seq start end))
  (:documentation
   "Equivalent to CL:SUBSEQ, but supports the BUFFER type."))

(defgeneric line-count (buffer)
  (:method ((string string))
    (1+ (count #\Newline string)))
  (:documentation
   "Returns the number of lines in BUFFER (i.e. 1+ the number of #\Newline's)."))

(defgeneric line-number-offset (buffer line-number)
  (:method ((string string) line-number)
    (if (= line-number 1)
        0
        (loop :with line = 1
              :for offset from 0
              :for c across string
              :do (when (and (char= c #\Newline) (>= (incf line) line-number))
                    (return (1+ offset)))
              :finally (if (= (1+ line) line-number)
                           (length string)
                           (error "line-number too large")))))
  (:documentation
   "Returns the offset of the first character in the LINE-NUMBERth line in BUFFER.
If (1+ (line-count buffer)) is passed as the second argument, the LENGTH of the buffer will
be returned. If LINE-NUMBER exceeds the number of lines in BUFFER, raise an condition of
type VICO-BOUNDS-ERROR."))

(defgeneric offset-in-bytes (buffer offset)
  (:method ((string string) offset)
    (babel:string-size-in-octets
     (make-array offset :displaced-to string :element-type (array-element-type string))))
  (:documentation
   "Returns OFFSET into BUFFER in terms of bytes when encoded in UTF-8 (optional)."))

(defgeneric byte-length (buffer)
  (:method ((string string))
    (babel:string-size-in-octets string))
  (:documentation
   "Returns the byte length of BUFFER when encoded in UTF-8. Equivalent to (offset-in-bytes
buffer (length buffer)). (optional)."))

;; writers TODO implement insert,erase,write-to-file for strings

(defgeneric insert (buffer string offset)
  (:documentation
   "Inserts STRING (of type STRING) into BUFFER at OFFSET. Returns no values."))

(defgeneric erase (buffer start &optional end)
  (:documentation
   "Erases the characters in BUFFER between the offsets START and END. Returns the deleted
string. END should default to the offset of the next character following START."))

(defgeneric undo (buffer)
  (:documentation
   "Undo the most recent edit to BUFFER. Returns BUFFER."))

(defgeneric redo (buffer)
  (:documentation
   "Redo the most recent edit to BUFFER. Returns BUFFER."))

(defgeneric write-to-file (buffer pathname &key start end)
  (:method ((buffer buffer) pathname &key (start 0) end)
    (with-open-file (stream pathname :direction :output
                                     :element-type 'character
                                     :if-does-not-exist :create)
      (write-sequence (subseq buffer start end) stream)))
  (:documentation
   "Write the contents of BUFFER bounded by offsets START, END to the file specified by
PATHNAME in an efficient manner (optional)."))

(defgeneric keybinds (buffer)
  (:method ((buffer buffer))
    nil)
  (:documentation "Returns an alist of keybindings associated with BUFFER."))
