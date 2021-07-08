;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; buffer interface
;;

(defpackage :vico-core.buffer
  (:use :cl)
  (:local-nicknames (:graphemes :vico-core.graphemes)
                    (:conditions :vico-core.conditions))
  (:shadow :byte :char :subseq)
  (:export #:buffer #:make-buffer #:bufferp #:copy-buffer #:close-buffer
           #:size #:line-count #:char-count
           #:byte #:char #:subseq
           #:line-number-index
           #:insert #:erase
           #:write-to-octet-stream
           #:undo #:redo
           #:begin-undo-group #:end-undo-group #:with-undo-group
           ;; misc
           #:filename
           #:edit-timestamp
           ;; cursor
           #:cursor #:make-cursor #:cursorp #:copy-cursor
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

           #:cursor-next-grapheme #:cursor-prev-grapheme
           #:move-cursor-graphemes #:move-cursor-graphemes*

           #:cursor-bol #:cursor-eol
           #:cursor-find-next #:cursor-find-prev
           #:cursor-search-next #:cursor-search-prev
           ;; spans
           #:span
           #:span-start #:span-end

           ;; additional metadata
           #:local-binds
           ))
(in-package :vico-core.buffer)

(defclass buffer ()
  ((%local-binds :initarg :local-binds
                 :initform (make-hash-table :synchronized t)
                 :reader local-binds))
  (:documentation "All base buffers (and probably mixins) must inherit this class."))

(defun bufferp (object)
  (typep object 'buffer))

(defgeneric make-buffer (type &key initial-contents initial-stream &allow-other-keys)
  (:documentation
   "Constructor for buffers, dispatching on the symbol TYPE. In a general sense, buffers
are anything that represent an ordered sequence of bytes. All buffers should define a
method on INITIALIZE-INSTANCE accepting the keyword parameter :INITIAL-CONTENTS for
specifying the contents of the buffer (in an unspecified format). :INITIAL-STREAM may
also be optionally accepted, indicating that the provided octet stream's contents should
be used to initialize the buffer. This overrides INITIAL-CONTENTS.
All operations on the buffer may only be performed on the owning thread unless specified
to prevent surprising effects in user code. If any index passed is out of bounds or
splitting a codepoint, these functions should signal a condition of type
VICO-INDEX-ERROR. All indexes are in bytes unless otherwise specified."))

(defgeneric copy-buffer (buffer)
  (:documentation
   "Returns a copy of BUFFER that will persist its contents whilst the original is
   edited (by say, another thread). This operation itself is thread-safe."))

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
  (:method ((buffer buffer) start &optional end)
    (setf end (or end (size buffer)))
    (with-output-to-string (s)
      (handler-case
          (loop :with cursor = (make-cursor buffer start)
                :until (= (index-at cursor) end)
                :do (write-char (char-at cursor) s)
                    (cursor-next-char cursor))
        (conditions:vico-bad-index ()))))
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
   specified by STREAM in an efficient manner. If STREAM is a file-stream then this may be
considered a request to 'save' the buffer."))

(defgeneric undo (buffer)
  (:documentation
   "Undo the most recent edit to BUFFER. Returns T if there was anything to undo."))

(defgeneric redo (buffer)
  (:documentation
   "Redo the most recent edit to BUFFER. Returns T if there was anything to redo."))

(defgeneric begin-undo-group (buffer))
(defgeneric end-undo-group (buffer))

(defmacro with-undo-group (buffer &body body)
  `(unwind-protect
        (progn
          (begin-undo-group ,buffer)
          ,@body)
     (end-undo-group ,buffer)))

(defgeneric filename (buffer))

(defgeneric edit-timestamp (buffer)
  (:documentation
   "Returns some form of non-decreasing identifier corresponding uniquely to the current
buffer contents."))

;; cursors are for monitoring byte & line indexes into a buffer
;; they remain a strictly low-level (composable) iteration mechanism for now
;; they are NOT required to be thread safe - COPY-CURSOR exists for multithreaded usage

(defgeneric make-cursor (buffer index &key track static &allow-other-keys)
  (:documentation "Returns an appropriate cursor for BUFFER's type at INDEX - should be a
subtype of CURSOR. This operation is thread-safe."))

(defgeneric copy-cursor (cursor)
  (:documentation "Returns a copy of cursor. This operation is thread safe."))

(defvar *cursor-types* nil
  "A list of cursor types. This is to allow more efficient structure impls.")

(defun cursorp (object)
  (loop :for type in *cursor-types*
        :thereis (typep object type)))

(deftype cursor () `(satisfies cursorp))

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
;; Any operation with the cursor after closing will signal VICO-CURSOR-INVALID

(defgeneric cursor-tracked-p (cursor))
(defgeneric (setf cursor-tracked-p) (new-value cursor)
  (:documentation "This causes CURSOR to be tracked by its buffer and is *not* thread
safe."))

(defgeneric index-at (cursor))

(defgeneric line-at (cursor))

(defgeneric byte-at (cursor)
  (:documentation "Returns the byte at CURSOR's position."))

(defgeneric char-at (cursor)
  (:documentation "Returns the character at CURSOR's position."))

(defgeneric insert-at (cursor string)
  (:documentation "Inserts the string STRING at CURSOR's position."))

(defgeneric delete-at (cursor &optional count)
  (:documentation "Deletes COUNT bytes at CURSOR's position."))

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
  (let* ((line (if (cursorp line)
                   (line-at line)
                   line))
         (delta (- line (line-at cursor))))
    (if (plusp delta)
        (cursor-next-line cursor delta)
        (cursor-prev-line cursor (- delta)))))

(defun cursor-next-grapheme (cursor &optional (count 1))
  (let ((buffer (cursor-buffer cursor)))
    (let ((searcher (graphemes:make-grapheme-searcher
                     buffer
                     :length (size buffer)
                     :accessor (let ((prev 0))
                                 (lambda (buffer index)
                                   (declare (ignore buffer))
                                   (cursor-next-char cursor (- index prev))
                                   (setf prev index)
                                   (char-at cursor))))))
      (dotimes (i count)
        (graphemes:next-grapheme searcher)))
    cursor))

(defun cursor-prev-grapheme (cursor &optional (count 1))
  (unless (zerop count) ;this may move
    (let ((buffer (cursor-buffer cursor)))
      (let ((searcher (graphemes:make-grapheme-searcher
                       buffer
                       :from-end t
                       :length (index-at cursor)
                       :accessor (let ((prev (index-at cursor)))
                                   (lambda (buffer index)
                                     (declare (ignore buffer))
                                     (cursor-prev-char cursor (- prev index))
                                     (setf prev index)
                                     (char-at cursor))))))
        (dotimes (i count)
          (graphemes:next-grapheme searcher)))
      cursor)))

(defun move-cursor-graphemes (cursor count)
  (if (plusp count)
      (cursor-next-grapheme cursor count)
      (cursor-prev-grapheme cursor (- count))))

(defun move-cursor-graphemes* (cursor count)
  (if (plusp count)
      (let ((buffer (cursor-buffer cursor)))
        (handler-case
            (cursor-next-grapheme cursor count)
          (conditions:vico-bad-index ()
            (move-cursor-to cursor (size buffer)))))
      (handler-case
          (cursor-prev-grapheme cursor (- count))
        (conditions:vico-bad-index ()
          (move-cursor-to cursor 0)))))

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

;;(declaim (inline cursor-bol cursor-eol))
(defun cursor-bol (cursor)
  (if (cursor-find-prev cursor #\newline)
      (cursor-next cursor)
      (move-cursor-to cursor 0)))

(defun cursor-eol (cursor)
  (unless (cursor-find-next cursor #\newline)
    (move-cursor-to cursor (size (cursor-buffer cursor)))))

;; TODO interface for simple byte searches, separate from regex

(defgeneric cursor-search-next (cursor string &optional max-chars)
  (:documentation "Returns CURSOR and the length of the match as multiple values if found."))
(defgeneric cursor-search-prev (cursor string &optional max-chars)
  (:documentation "Returns CURSOR and the length of the match as multiple values if found."))

;; spans

(defclass span ()
  ((start :initarg :start
          :initform (error "no span START")
          :accessor span-start
          :type cursor)
   (end :initarg :end
        :initform (error "no span END")
        :accessor span-end
        :type cursor)))
