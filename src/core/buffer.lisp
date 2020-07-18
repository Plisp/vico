;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; buffer interface
;;

(defpackage :vico-core.buffer
  (:use :cl)
  (:local-nicknames (:conditions :vico-core.conditions))
  (:shadow :byte :char :subseq)
  (:export #:buffer #:make-buffer #:copy-buffer #:close-buffer
           #:size #:line-count #:char-count
           #:byte #:char #:subseq
           #:line-number-index
           #:insert #:erase
           #:write-to-octet-stream
           #:undo #:redo
           ;;XXX move this stuff out of here - advice
           #:buffer-name #:keybinds #:edit-timestamp
           ;; cursor TODO document
           #:cursor #:make-cursor #:copy-cursor #:cursorp
           #:cursor-buffer
           #:cursor-static-p
           #:cursor-tracked-p

           #:cursor= #:cursor/=
           #:cursor< #:cursor> #:cursor<= #:cursor>=
           #:cursor- #:cursor+

           #:index-at #:line-at
           #:byte-at #:char-at #:subseq-at

           #:insert-at #:delete-at

           #:cursor-next #:cursor-prev
           #:move-cursor #:move-cursor* #:move-cursor-to

           #:cursor-next-char #:cursor-prev-char
           #:move-cursor-chars #:move-cursor-chars* #:move-cursor-to-char

           #:cursor-next-line #:cursor-prev-line
           #:move-cursor-lines #:move-cursor-lines* #:move-cursor-to-line

           #:cursor-bol #:cursor-eol
           #:cursor-find-next #:cursor-find-prev
           #:cursor-search-next #:cursor-search-prev

           ;; conditions
           #:signal-bad-cursor-index
           #:signal-bad-cursor-line
           ))
(in-package :vico-core.buffer)

(defclass buffer () ()) ;; TODO obsolete

(defgeneric make-buffer (type &key initial-contents initial-stream &allow-other-keys)
  (:documentation
   "Constructor for buffers, dispatching on the symbol TYPE. In a general sense, buffers
are anything that represent an ordered sequence of bytes. All buffers should define a
method on INITIALIZE-INSTANCE accepting the keyword parameter :INITIAL-CONTENTS for
specifying the contents of the buffer (in an unspecified format). :INITIAL-STREAM may
also be optionally accepted, indicating that the provided octet stream's contents should
be used to initialize the buffer. This overrides INITIAL-CONTENTS.
All destructive operations on the buffer may only be performed on the owning thread
to prevent surprising effects in user code. If any index passed is out of bounds or
splitting a codepoint, these functions should signal a condition of type
VICO-INDEX-ERROR. All indexes are in bytes unless otherwise specified."))

(defgeneric copy-buffer (buffer)
  (:documentation
   "Returns a copy of BUFFER that will persist its contents whilst the original is
   edited (by say, another thread). This operation itself must be thread-safe."))

(defgeneric close-buffer (buffer)
  (:documentation
   "Cleans up resources associated with BUFFER. Called when BUFFER is removed from the
editor's BUFFERS list. Subsequent calls on the same BUFFER should be a no-op. May only be
called from the owning thread.
Threaded users should hook onto this to discard their remaining CURSORS referencing
BUFFER."))

;; readers

(defgeneric size (buffer)
  (:documentation
   "Returns the length of BUFFER in bytes."))

(defgeneric byte (buffer n)
  (:documentation "Retrieve the byte at index N into BUFFER."))

(defgeneric char (buffer n)
  (:documentation "Returns the codepoint (as a CHARACTER) at index N where N is a *byte
offset* from the start of the buffer.  This will work as expected when you aren't dealing
with unicode. In the case that you are, it's recommended to use a cursor. Returns the
codepoint (as a CHARACTER) at index N."))

(defgeneric subseq (buffer start &optional end)
  (:documentation
   "Equivalent to CL:SUBSEQ, but supports the BUFFER type. START and END are indexes. See
   documentation for CHAR."))

(defgeneric line-count (buffer) ;TODO I might support different line endings later *sigh*
  (:documentation
   "Returns the number of lines in BUFFER (i.e. 1+ the number of #\Newline's)."))

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
will be returned. If LINE-NUMBER exceeds the number of lines in BUFFER, raise an
condition of type VICO-BOUNDS-ERROR."))

(defgeneric char-count (buffer)
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
  (:documentation
   "Returns some form of increasing timestamp corresponding uniquely to the last edit."))

;; cursors are for monitoring byte & line indexes into a buffer
;; they remain a strictly low-level (composable) iteration mechanism for now
;; they are NOT required to be thread safe - COPY-CURSOR exists for multithreaded usage

(defgeneric make-cursor (buffer index &key track static)
  (:documentation "Returns an appropriate cursor for BUFFER's type at INDEX - should be a
subtype of CURSOR."))

(defgeneric copy-cursor (cursor)
  (:documentation "Returns a copy of cursor. Thread safe."))

(defvar *cursor-types* nil
  "A list of cursor types. This is to allow more efficient structure impls.")

(defun cursorp (object)
  (loop :for type in *cursor-types*
        :thereis (typep object type)))

(defgeneric cursor= (cursor1 cursor2))
(defgeneric cursor/= (cursor1 cursor2))
(defgeneric cursor< (cursor1 cursor2))
(defgeneric cursor> (cursor1 cursor2))
(defgeneric cursor<= (cursor1 cursor2))
(defgeneric cursor>= (cursor1 cursor2))
(defgeneric cursor- (cursor1 cursor2))
(defgeneric cursor+ (cursor1 cursor2))

(defgeneric cursor-buffer (cursor))

;; on insertions *at* their index, static cursors will not be moved
(defgeneric cursor-static (cursor))
(defgeneric (setf cursor-static) (new-value cursor))

;; tracked cursors are owned by the buffer's owning thread
;; copies should be used when manipulating from multiple threads, they will be untracked
;; untracked cursors will be invalidated on edits, signalling VICO-CURSOR-INVALID on
;; their next access or when their current access completes

;; (if you do not UNTRACK-CURSOR, it will be leaked until the buffer is gc'd)
;; (that's mostly fine, but make sure you release references to cursors when
;; buffers are closed, otherwise the buffer will be kept alive indefinitely)

(defgeneric cursor-tracked-p (cursor))
(defgeneric (setf cursor-tracked-p) (new-value cursor))

(defgeneric index-at (cursor))
(defgeneric line-at (cursor))

(defgeneric byte-at (cursor))
(defgeneric char-at (cursor))
(defgeneric insert-at (cursor string))
(defgeneric delete-at (cursor &optional count))

(defgeneric cursor-next (cursor &optional count))
(defgeneric cursor-prev (cursor &optional count))

(defun move-cursor (cursor count)
  (if (plusp count)
      (cursor-next cursor count)
      (cursor-prev cursor (- count))))

(defun move-cursor* (cursor count)
  "Does not error, moves the cursor as far as it can"
  (handler-case
      (move-cursor cursor count)
    (conditions:vico-bad-index ()
      (if (plusp count)
          (move-cursor-to cursor (size (cursor-buffer cursor)))
          (move-cursor-to cursor 0)))))

(defun move-cursor-to (cursor index)
  (let* ((index (if (cursorp index)
                    (index-at index)
                    index))
         (delta (- index (index-at cursor))))
    (if (plusp delta)
        (cursor-next cursor delta)
        (cursor-prev cursor (- delta)))))

;; these are not meaningful in binary files, make this clear
(defgeneric cursor-next-char (cursor &optional count))
(defgeneric cursor-prev-char (cursor &optional count))

(defun move-cursor-chars (cursor count)
  (if (plusp count)
      (cursor-next-char cursor count)
      (cursor-prev-char cursor (- count))))

(defun move-cursor-chars* (cursor count)
  "Does not error, moves the cursor as far as it can"
  (handler-case
      (move-cursor-chars cursor count)
    (conditions:vico-bad-index ()
      (if (plusp count)
          (move-cursor-to cursor (size (cursor-buffer cursor)))
          (move-cursor-to cursor 0)))))

(defun move-cursor-to-char (cursor char)
  (move-cursor-to cursor 0)
  (cursor-next-char cursor char))

;; optimized routines for line traversal
(defgeneric cursor-next-line (cursor &optional count))
(defgeneric cursor-prev-line (cursor &optional count))

(defun move-cursor-lines (cursor count)
  (if (plusp count)
      (cursor-next-line cursor count)
      (cursor-prev-line cursor (- count))))

(defun move-cursor-lines* (cursor count)
  "Does not error, moves the cursor as far as it can"
  (handler-case
      (move-cursor-lines cursor count)
    (conditions:vico-bad-line-number ()
      (if (plusp count)
          (move-cursor-to cursor (size (cursor-buffer cursor)))
          (move-cursor-to cursor 0)))))

(defun move-cursor-to-line (cursor line)
  (let ((delta (- line (line-at cursor))))
    (if (plusp delta)
        (cursor-next-line cursor delta)
        (cursor-prev-line cursor (- delta)))))

(declaim (notinline subseq-at))
(defun subseq-at (cursor length) ; should this be generic?
  "Returns a string of LENGTH *codepoints*."
  (let ((copy (copy-cursor cursor))
        (buffer (make-array length :element-type 'character)))
    (loop :for i :below length
          :do (setf (aref buffer i) (char-at copy))
              (cursor-next-char copy)
          :finally (return buffer))))

;; these leave the cursor in its original position if nothing is found
(defgeneric cursor-find-next (cursor char))
(defgeneric cursor-find-prev (cursor char))

(declaim (inline cursor-bol cursor-eol))
(defun cursor-bol (cursor)
  (if (cursor-find-prev cursor #\newline)
      (cursor-next cursor)
      (move-cursor-to cursor 0)))

(defun cursor-eol (cursor)
  (unless (cursor-find-next cursor #\newline)
    (move-cursor-to cursor (size (cursor-buffer cursor)))))

(defgeneric cursor-search-next (cursor string))
(defgeneric cursor-search-prev (cursor string))

;;; conditions

(defun signal-bad-cursor-index (cursor index)
  (let ((buffer (cursor-buffer cursor)))
    (error 'conditions:vico-bad-index
           :buffer buffer
           :index index
           :bounds (cons 0 (size buffer)))))

(defun signal-bad-cursor-line (cursor line)
  (let ((buffer (cursor-buffer cursor)))
    (error 'vico-core.conditions:vico-bad-line-number
           :buffer buffer
           :line-number line
           :bounds (cons 0 (1+ (line-count buffer))))))
