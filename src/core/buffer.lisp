(defpackage :vico-core.buffer
  (:use :cl)
  (:local-nicknames (:conditions :vico-core.conditions))
  (:shadow :byte :char :length :subseq)
  (:export #:buffer #:copy-buffer #:close-buffer
           #:length
           #:byte
           #:char
           #:subseq
           #:line-count
           #:line-number-index
           #:char-length
           #:insert
           #:erase
           #:write-to-octet-stream
           #:undo #:redo
           #:buffer-name
           #:keybinds
           #:edit-timestamp
           #:cursor #:make-cursor #:copy-cursor #:cursor-valid-p
           #:cursor-buffer
           #:dirty-cursor
           #:byte-at #:char-at #:insert-at #:erase-at #:subseq-at
           #:index-at
           #:cursor-next #:cursor-prev #:move-cursor #:move-cursor-to
           #:line-at
           #:cursor-bol
           #:cursor-search-char-next #:cursor-search-char-prev
           #:cursor-next-line #:cursor-prev-line #:move-cursor-lines #:move-cursor-to-line
           #:update-cursor))
(in-package :vico-core.buffer)

(defclass buffer ()
  ()
  (:documentation
   "Base class for buffers. In a general sense, buffers are anything that represent an
ordered sequence of bytes.
All buffers should define a method on INITIALIZE-INSTANCE accepting the keyword parameter
:INITIAL-CONTENTS for specifying the contents of the buffer (in an unspecified format).
:INITIAL-FILE may also be optionally accepted, indicating that the provided pathname's
contents should be used to initialize the buffer. This overrides INITIAL-CONTENTS.
If any index passed is out of bounds or splitting a codepoint, these functions should signal
a condition of type VICO-INDEX-ERROR. All indexes are in bytes unless otherwise specified."))

(defvar *max-buffer-size* (expt 2 50)
  "For type declaration purposes (for in-memory data structures). No buffer should hold
more than a pebibyte (assuming bytes).")

(defgeneric copy-buffer (buffer) ;TODO implement backend
  (:documentation
   "Returns a copy of BUFFER that will persist its contents whilst the original is edited
(by say, another thread). This operation itself must be thread-safe."))

(defgeneric close-buffer (buffer)
  (:documentation
   "Cleans up resources associated with BUFFER. Called when BUFFER is removed from the
editor's BUFFERS list."))

;; readers

(defgeneric length (buffer)
  (:documentation
   "Returns the length of BUFFER in bytes. (mandatory)"))

(defgeneric byte (buffer n) ;TODO what sort of interface for raw bytes?
  (:documentation "Retrieve the byte at index N into BUFFER, may be more efficient than CHAR
with unibyte encodings."))

(defgeneric char (buffer n)
  (:documentation "N is a *byte offset* from the start of the buffer. This will work as
expected when you aren't dealing with unicode. In the case that you are, it's recommended
to use a cursor. Returns the codepoint (as a CHARACTER) at index N. (mandatory)"))

(defgeneric subseq (buffer start &optional end)
  (:documentation
   "Equivalent to CL:SUBSEQ, but supports the BUFFER type. START and END are *byte offsets*
. See documentation for CHAR. (mandatory)"))

(defgeneric line-count (buffer) ;TODO I might support different line endings later *sigh*
  (:documentation
   "Returns the number of lines in BUFFER (i.e. 1+ the number of #\Newline's).
(mandatory)"))

(defgeneric line-number-index (buffer line-number)
  ;; (:method ((string string) line-number)
  ;;   (if (= line-number 1)
  ;;       0
  ;;       (loop :with line = 1
  ;;             :for index from 0
  ;;             :for c across string
  ;;             :do (when (and (char= c #\Newline) (>= (incf line) line-number))
  ;;                   (return (1+ index)))
  ;;             :finally (if (= (1+ line) line-number)
  ;;                          (return (length string))
  ;;                          (error "line-number out of range")))))
  (:documentation
   "Returns the index of the first byte in the LINE-NUMBERth line in BUFFER.
If (1+ (line-count buffer)) is passed as the second argument, the LENGTH of the buffer
will be returned. If LINE-NUMBER exceeds the number of lines in BUFFER, raise an condition
of type VICO-BOUNDS-ERROR."))

(defgeneric char-length (buffer)
  (:documentation
   "Returns the length of BUFFER in UTF-8 codepoints or CHARACTERs."))

;; writers

(defgeneric insert (buffer string index)
  (:documentation
   "Inserts STRING (of type STRING) into BUFFER at INDEX. Returns no values."))

(defgeneric erase (buffer start &optional count)
  (:documentation
   "Erases COUNT (default 1) bytes in BUFFER starting from START. Returns no values."))

(defgeneric write-to-octet-stream (buffer stream &key start end)
  (:documentation
   "Write the contents of BUFFER bounded by indexes START, END to the octet stream
specified by STREAM in an efficient manner."))

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

(defgeneric edit-timestamp (buffer)
  (:documentation "Returns some form of timestamp corresponding uniquely to the last edit."))

;; cursors are for monitoring byte,line indexes into a buffer, specialize other methods
;; they remain a strictly low-level (composable) iteration mechanism for now
;; must be thread safe

(defgeneric make-cursor (buffer index)
  (:documentation "Returns an appropriate cursor for BUFFER's type at INDEX - should be a
subtype of CURSOR."))

(defgeneric copy-cursor (cursor)
  (:documentation "Returns a copy of cursor."))

(defgeneric cursor-valid-p (cursor))
(defgeneric dirty-cursor (cursor))

(defgeneric cursor-buffer (cursor))

(defgeneric byte-at (cursor)) ;;TODO implement backend
(defgeneric char-at (cursor))
(defgeneric insert-at (cursor string))
(defgeneric erase-at (cursor &optional count))

(defgeneric cursor-next (cursor &optional count))
(defgeneric cursor-prev (cursor &optional count))
(defgeneric index-at (cursor))
(defgeneric (setf index-at) (new-value cursor))

(declaim (inline move-cursor move-cursor-to))
(defun move-cursor (cursor count)
  (if (plusp count)
      (cursor-next cursor count)
      (cursor-prev cursor (- count))))
(defun move-cursor-to (cursor index)
  (let ((old-index (index-at cursor)))
    (if (> index old-index)
        (cursor-next cursor (- index old-index))
        (cursor-prev cursor (- old-index index)))))

;;(defgeneric cursor-search-next (cursor string))
;;(defgeneric cursor-search-prev (cursor string))

(defgeneric cursor-search-char-next (cursor char))
(defgeneric cursor-search-char-prev (cursor char))

(defgeneric cursor-next-line (cursor &optional count))
(defgeneric cursor-prev-line (cursor &optional count))
(defgeneric line-at (cursor))

(defgeneric cursor-bol (cursor))
;; (declaim (inline cursor-bol))
;; (defun cursor-bol (cursor)
;;   (when (cursor-search-char-prev cursor #\newline)
;;     (cursor-next cursor)))

(declaim (inline move-cursor-lines move-cursor-to-line))
(defun move-cursor-lines (cursor count)
  (if (plusp count)
      (cursor-next-line cursor count)
      (cursor-prev-line cursor (- count))))
(defun move-cursor-to-line (cursor line)
  (let ((old-line (line-at cursor)))
    (if (> line old-line)
        (cursor-next-line cursor (- line old-line))
        (cursor-prev-line cursor (- old-line line)))))

(declaim (inline subseq-at))
(defun subseq-at (cursor length) ; should this be generic hmmm
  (let ((buffer (make-array length :element-type 'character)))
    (loop :for i :below length
          :do (setf (aref buffer i) (char-at cursor))
              (cursor-next cursor)
          :finally (cursor-prev cursor length)
                   (return buffer))))

;; also specialize buffer CHAR, INSERT, ERASE for buffer

(defgeneric update-cursor (cursor)
  (:documentation "Call before accesses if cursor is dirtied."))
