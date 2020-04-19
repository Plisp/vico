(defpackage :vico-core.buffer
  (:use :cl)
  (:local-nicknames (:conditions :vico-core.conditions))
  (:shadow :char :length :subseq)
  (:export #:buffer #:copy-buffer
           #:length
           #:char
           #:subseq
           #:line-count
           #:line-number-index
           #:index-in-bytes
           #:byte-length
           #:insert
           #:erase
           #:write-to-file
           #:undo #:redo
           #:buffer-name
           #:keybinds
           #:cursor #:make-cursor #:copy-cursor
           #:cursor-buffer #:cursor-dirty-p
           #:char-at #:insert-at #:erase-at #:subseq-at
           #:index-at
           #:cursor-next #:cursor-prev #:cursor-move #:cursor-move-to
           #:line-at
           #:cursor-next-line #:cursor-prev-line #:cursor-move-line #:cursor-move-to-line
           #:update-cursor))
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
If any index passed is out of bounds, these functions should signal a condition of type
VICO-BOUNDS-ERROR."))

(defvar *max-buffer-size* (expt 2 50)
  "For type declaration purposes (for in-memory data structures). No buffer should hold
more than a pebibyte (assuming bytes).")

(defgeneric copy-buffer (buffer)
  (:documentation
   "Returns a copy of BUFFER that will persist its contents whilst the original is edited
(by say, another thread). This operation itself must be thread-safe."))

;; readers TODO provide defaults impls for everything that's not mandatory

(defgeneric length (buffer)
  (:method ((seq sequence))
    (cl:length seq))
  (:documentation
   "Returns the length of BUFFER in characters. (mandatory)"))

(defgeneric char (buffer n)
  (:method ((seq string) n)
    (cl:char seq n)))

(defgeneric subseq (buffer start &optional end)
  (:method ((seq sequence) start &optional end)
    (cl:subseq seq start end))
  (:documentation
   "Equivalent to CL:SUBSEQ, but supports the BUFFER type. (mandatory)"))

(defgeneric line-count (buffer) ;TODO I might support different line endings later *sigh*
  (:method ((string string))
    (1+ (count #\Newline string)))
  (:documentation
   "Returns the number of lines in BUFFER (i.e. 1+ the number of #\Newline's).
(mandatory)"))

(defgeneric line-number-index (buffer line-number)
  (:method ((string string) line-number)
    (if (= line-number 1)
        0
        (loop :with line = 1
              :for index from 0
              :for c across string
              :do (when (and (char= c #\Newline) (>= (incf line) line-number))
                    (return (1+ index)))
              :finally (if (= (1+ line) line-number)
                           (return (length string))
                           (error "line-number out of range")))))
  (:documentation
   "Returns the index of the first character in the LINE-NUMBERth line in BUFFER.
If (1+ (line-count buffer)) is passed as the second argument, the LENGTH of the buffer
will be returned. If LINE-NUMBER exceeds the number of lines in BUFFER, raise an condition
of type VICO-BOUNDS-ERROR."))

(defgeneric index-in-bytes (buffer index)
  (:method ((string string) index)
    (babel:string-size-in-octets
     (make-array index :displaced-to string :element-type (array-element-type string))))
  (:documentation
   "Returns INDEX into BUFFER in terms of bytes when encoded in UTF-8."))

(defgeneric byte-length (buffer)
  (:method ((string string))
    (babel:string-size-in-octets string))
  (:documentation
   "Returns the byte length of BUFFER when encoded in UTF-8. Equivalent to
(index-in-bytes buffer (length buffer))."))

;; writers TODO implement insert,erase,write-to-file for strings maybe

(defgeneric insert (buffer string index)
  (:documentation
   "Inserts STRING (of type STRING) into BUFFER at INDEX. Returns no values."))

(defgeneric erase (buffer start &optional count)
  (:documentation
   "Erases COUNT (default 1) characters in BUFFER starting from START. Returns no values."))

(defgeneric write-to-file (buffer pathname &key start end)
  (:method ((buffer buffer) pathname &key (start 0) end)
    (with-open-file (stream pathname :direction :output
                                     :element-type 'character
                                     :if-does-not-exist :create)
      (write-sequence (subseq buffer start end) stream)))
  (:documentation
   "Write the contents of BUFFER bounded by indexs START, END to the file specified by
PATHNAME in an efficient manner."))

;; below methods are not required for backends

(defgeneric undo (buffer)
  (:documentation
   "Undo the most recent edit to BUFFER. Returns BUFFER."))

(defgeneric redo (buffer)
  (:documentation
   "Redo the most recent edit to BUFFER. Returns BUFFER."))

(defgeneric buffer-name (buffer))

(defgeneric keybinds (buffer)
  (:method ((buffer buffer))
    nil)
  (:documentation "Returns an alist of BUFFER-local keybindings."))

;; cursors are for monitoring char,line indexes into a buffer, specialize other methods
;; cursor-buffer-mixin auto-updates indexs & dirties cursors on edits
;; they remain a strictly low-level (composable) iteration mechanism for now
;; must be thread safe

(defgeneric make-cursor (buffer index)
  (:documentation "Returns an appropriate cursor for BUFFER's type at INDEX - should be a
subtype of CURSOR."))

(defgeneric copy-cursor (cursor)
  (:documentation "Returns a copy of cursor."))

(defgeneric cursor-buffer (cursor))
(defgeneric cursor-dirty-p (cursor))
(defgeneric (setf cursor-dirty-p) (cursor new-value))

(defgeneric char-at (cursor))
(defgeneric insert-at (cursor))
(defgeneric erase-at (cursor))

(defgeneric cursor-next (cursor &optional count))
(defgeneric cursor-prev (cursor &optional count))
(defgeneric index-at (cursor))
(defgeneric (setf index-at) (cursor new-value))

(defun cursor-move (cursor count)
  (if (plusp count)
      (cursor-next cursor count)
      (cursor-prev cursor (- count))))

(defun cursor-move-to (cursor index)
  (let ((old-index (index-at cursor)))
    (if (> index old-index)
        (cursor-next cursor (- index old-index))
        (cursor-prev cursor (- old-index index)))))

(defgeneric cursor-next-line (cursor &optional count))
(defgeneric cursor-prev-line (cursor &optional count))
(defgeneric line-at (cursor))

(defun cursor-move-line (cursor count)
  (if (plusp count)
      (cursor-next-line cursor count)
      (cursor-prev-line cursor (- count))))

(defun cursor-move-to-line (cursor line)
  (let ((old-line (line-at cursor)))
    (if (> line old-line)
        (cursor-next-line cursor (- line old-line))
        (cursor-prev-line cursor (- old-line line)))))

;; TODO determine need to dispatch?
(defun subseq-at (cursor length)
  (let ((copy (copy-cursor cursor))
        (buffer (make-array length :element-type 'character)))
    (loop :for i :below length
          :do (setf (aref buffer i) (char-at copy))
              (cursor-next copy)
          :finally (return buffer))))

;; also specialize buffer CHAR, INSERT, ERASE for buffer

(defgeneric update-cursor (cursor)
  (:documentation "call before accesses if cursor is dirtied. update according to INDEX. up to implementation"))
