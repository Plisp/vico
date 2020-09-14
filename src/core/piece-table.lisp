;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; piece table rewrite
;;
;; DONE implement interface
;; DONE mmap()
;; DONE error handling
;; DONE undo
;; DONE proper data buffer types
;; TODO tests
;; TODO encoding support with babel
;;

(defpackage :vico-core.buffer.piece-table
  (:use :cl :alexandria)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:io :vico-core.io)
                    (:enc :babel-encodings)
                    (:ffi :cffi)))
(in-package :vico-core.buffer.piece-table)

;; credit: from the mmap library by Shinmera
(cffi:defctype size-t
  #+64-bit :uint64
  #+32-bit :uint32)

(deftype idx () 'fixnum)

;; if we're gonna abbreviate, go all the way
(declaim (inline inc-ptr))
(defun inc-ptr (ptr offset)
  (ffi:inc-pointer ptr offset))

(defconstant +min-data-buffer-size+ (expt 2 12)
  "the page size on most x64 platforms?")

(defstruct (data-buffer
            (:print-object
             (lambda (db stream)
               (let* ((enc::*suppress-character-coding-errors* t)
                      (contents
                        (ffi:foreign-string-to-lisp (data-buffer-data db)
                                                    :count (data-buffer-size db)
                                                    :max-chars 100)))
                 (format stream "#S(DATA-BUFFER :size ~d :capacity ~d ~
                                    :type ~a contains ~a~@[...~])"
                         (data-buffer-size db)
                         (data-buffer-capacity db)
                         (type-of db)
                         contents
                         (= (length contents) 100))))))
  "manages a pointer to a foreign vector of octets"
  (size 0 :type idx)
  (capacity 0 :type idx :read-only t)
  (data (ffi:null-pointer) :type ffi:foreign-pointer :read-only t))

(defstruct (alloc-buffer (:include data-buffer))
  )

(defstruct (mmap-buffer (:include data-buffer))
  (addr (ffi:null-pointer) :type ffi:foreign-pointer :read-only t)
  (fd -1 :type fixnum))

(defstruct (static-buffer (:include data-buffer))
  (vector (static-vectors:make-static-vector 0) :type array))

(defun data-buffer-free (data-buffer)
  (etypecase data-buffer
    (alloc-buffer
     (ffi:foreign-string-free (data-buffer-data data-buffer)))
    (mmap-buffer
     (mmap:munmap (mmap-buffer-addr data-buffer)
                  (mmap-buffer-fd data-buffer)
                  (mmap-buffer-size data-buffer)))
    (static-buffer
     (static-vectors:free-static-vector (static-buffer-vector data-buffer)))))

(defstruct (piece (:print-object
                   (lambda (piece stream)
                     (let* ((enc::*suppress-character-coding-errors* t)
                            (contents
                              (ffi:foreign-string-to-lisp (piece-data piece)
                                                          :count (piece-size piece)
                                                          :max-chars 100)))
                       (format stream "#S(PIECE :size ~d contains:~a~@[...~])"
                               (piece-size piece)
                               contents
                               (= (length contents) 100))))))
  "SIZE is in octets"
  prev next
  (data (ffi:null-pointer) :type ffi:foreign-pointer)
  (size 0 :type idx))

(defclass piece-table-buffer (buf:buffer)
  ((piece-table :initarg :piece-table
                :type piece-table)))

(defstruct (piece-table (:conc-name pt-))
  (size 0 :type idx)
  (data-buffers (list) :type list)
  (end-cache nil :type (or null piece))
  (sentinel-start (make-piece) :read-only t)
  (sentinel-end (make-piece) :read-only t)
  (tracked-cursors (make-array 2 :fill-pointer 0 :adjustable t) :type vector)
  ;; must be held around accesses to the buffer's pieces/data-buffers/tracked-cursors to
  ;; safeguard the the integrity of the TRACKED-CURSORS vector and also the correctness
  ;; of created cursors - MAKE-CURSOR must be thread-safe
  (lock nil :type t)
  ;; used to validate fallible *read* access by untracked cursors in other threads
  (revision 0 :type #+ecl fixnum #+sbcl sb-ext:word #+ccl t)
  (closed-p nil :type boolean)
  (undo-stack (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (undo-position 0 :type idx) ; index into UNDO-STACK
  (undo-grouped-p nil :type boolean)
  (source nil :type (or null stream pathname))) ; TODO do we need types?

(defmacro lock-spinlock (place)
  `(loop :until (atomics:cas ,place nil t)))
(defmacro unlock-spinlock (place)
  `(loop :until (atomics:cas ,place t nil)))

(defmacro with-pt-lock ((piece-table) &body body)
  `(unwind-protect
        (progn
          (lock-spinlock (pt-lock ,piece-table))
          ,@body)
     (unlock-spinlock (pt-lock ,piece-table))))

;; initialize a bad line number index lazily, as this is potentially expensive
(define-condition vico-piece-table-bad-line-number (conditions:vico-bad-line-number)
  ((overrun :initarg :overrun
            :reader vico-piece-table-bad-line-overrun)
   (line-count :initarg :line-count
               :initform nil
               :reader vico-piece-table-bad-line-count)))

(defmethod conditions:buffer-bounds-error-bounds :before
    ((condition vico-piece-table-bad-line-number))
  (unless (slot-value condition 'conditions::bounds)
    (let ((line-count
            (or (vico-piece-table-bad-line-count condition)
                (pt-line-count (conditions:buffer-bounds-error-buffer condition)))))
      (setf (slot-value condition 'conditions::bounds) (list* 1 (1+ line-count))))))

(defmethod conditions:buffer-bounds-error-line-number :before
    ((condition vico-piece-table-bad-line-number))
  (unless (slot-value condition 'conditions::line-number)
    (let ((overrun (vico-piece-table-bad-line-overrun condition)))
      (setf (slot-value condition 'conditions::line-number)
            (if (minusp overrun)
                (1+ overrun) ; 0 is invalid
                (+ (or (vico-piece-table-bad-line-count condition)
                       (pt-line-count (conditions:buffer-bounds-error-buffer condition)))
                   overrun
                   1))))))

(defmethod buf:make-buffer ((type (eql :piece-table)) &key (initial-contents "")
                                                        initial-stream)
  (let (init-buffer char* file-length filename)
    (if initial-stream
        (progn
          (when (io:file-stream-p initial-stream)
            (setf filename (pathname initial-stream)
                  file-length (trivial-file-size:file-size-in-octets filename)))
          (if (or (not filename) (<= file-length +min-data-buffer-size+))
              (let ((vector (static-vectors:make-static-vector file-length)))
                (read-sequence vector initial-stream)
                (setf char* (static-vectors:static-vector-pointer vector)
                      init-buffer (make-static-buffer :size file-length
                                                      :capacity file-length
                                                      :vector vector)))
              (let (fd)
                (multiple-value-setq (char* fd)
                  (mmap:mmap filename :size file-length :mmap '(:shared)))
                (setf init-buffer (make-mmap-buffer :size file-length
                                                    :capacity file-length
                                                    :addr char*
                                                    :fd fd)))))
        (progn
          (multiple-value-setq (char* file-length)
            (ffi:foreign-string-alloc initial-contents :null-terminated-p t))
          (decf file-length) ; null terminator variable length, encodings
          (setf init-buffer (make-alloc-buffer :size file-length
                                               :capacity file-length
                                               :data char*))))
    ;;
    (let* ((init-piece (when (plusp file-length)
                         (make-piece :size file-length :data char*)))
           (pt (make-piece-table :size file-length
                                 :data-buffers (list init-buffer)
                                 :source filename)))
      (setf (piece-next (pt-sentinel-start pt)) (or init-piece (pt-sentinel-end pt))
            (piece-prev (pt-sentinel-end pt)) (or init-piece (pt-sentinel-start pt)))
      (when init-piece
        (setf (piece-prev init-piece) (pt-sentinel-start pt)
              (piece-next init-piece) (pt-sentinel-end pt)))
      (make-instance 'piece-table-buffer :piece-table pt))))

(defmethod buf:filename ((pt piece-table-buffer))
  (pt-source (slot-value pt 'piece-table)))

(defmethod buf:copy-buffer ((pt piece-table-buffer))
  ;;(copy-piece-table pt) ; deep copy required, be careful, is this even needed now?
  (error "not yet implemented"))

(defmethod buf:close-buffer ((pt piece-table-buffer))
  (let ((pt (slot-value pt 'piece-table)))
    (unless (pt-closed-p pt)
      (setf (pt-closed-p pt) t)
      ;; Here we invalidate all cursors, meaning no more future accesses to the buffer,
      ;; but it will be safe until other threads finish. Once cursors finish, references
      ;; to them should be dropped and the finalizer will run (eventually).
      (atomics:atomic-incf (pt-revision pt)) ; revision becomes odd - permanently invalid
      ;; readers may notice at their own pace, as the finalizer will not run yet
      (let ((data-buffers (pt-data-buffers pt)))
        (tg:finalize pt (lambda () (map () #'data-buffer-free data-buffers)))))))

(defmethod buf:size ((pt piece-table-buffer))
  (pt-size (slot-value pt 'piece-table)))

(declaim (ftype (function (ffi:foreign-pointer idx idx) idx) count-lfs))
(defun count-lfs (ptr start-offset end-offset)
  (declare (optimize speed (safety 0))
           (type idx start-offset end-offset))
  (loop :with lfs :of-type idx
        :with len :of-type idx = (- end-offset start-offset)
        :with start = (inc-ptr ptr start-offset)
        :do (let ((found (ffi:foreign-funcall "memchr"
                                              :pointer start
                                              :int #.(char-code #\newline)
                                              size-t len
                                              :pointer)))
              (when (ffi:null-pointer-p found)
                (return lfs))
              (incf lfs)
              (let ((delta (1+ (- (the idx (ffi:pointer-address found))
                                  (the idx (ffi:pointer-address start))))))
                (decf len delta)
                ;; this breaks on ccl, which likes deallocating FOUND for whatever reason
                ;; (setf start (inc-ptr found 1))
                (ffi:incf-pointer start delta)))))

(defun pt-line-count (pt)
  (declare (optimize speed))
  (loop :with line :of-type idx
        :for piece = (pt-sentinel-start pt) :then (piece-next piece)
        :while (piece-next piece)
        :do (incf line (count-lfs (piece-data piece) 0 (piece-size piece)))
        :finally (return line)))

(defmethod buf:line-count ((pt piece-table-buffer))
  (pt-line-count (slot-value pt 'piece-table)))

(defmethod buf:edit-timestamp ((pt piece-table-buffer))
  (pt-revision (slot-value pt 'piece-table)))

;;; cursor

;; regarding thread-safety - non-evloop-thread cursor access during editing:
;; modifying an invalid cursor's numeric fields is safe
;; at no point are cursor-prev, cursor-next (eql NIL) (maintain), so those are safe
;; accessing the piece data buffer is safe, append-only

;; cursors themselves are owned by a single thread and so no threading issues should exist,
;; except with COPY-CURSOR, which locks and is thus thread-safe
;; CURSOR-NEXT(-LINE), CURSOR-PREV(-LINE) lock as they mutate the cursor

;; n.b. when applicable, lock order is strictly pt-lock, cursor-lock

(defmacro with-cursor-lock ((cursor) &body body)
  "MUST be used around during cursor mutations"
  `(unwind-protect
        (progn
          (lock-spinlock (cursor-lock ,cursor))
          ,@body)
     (unlock-spinlock (cursor-lock ,cursor))))

(defstruct (cursor (:copier %copy-cursor)
                   (:print-object
                    (lambda (cursor stream)
                      (format stream "#S(CURSOR ~:[~;static ~]~:[untracked~;tracked~] ~
                                         :piece ~a :byte-offset ~d :index ~d :revision ~d)"
                              (cursor-static-p cursor)
                              (cursor-tracked-p cursor)
                              (cursor-piece cursor)
                              (cursor-byte-offset cursor)
                              (cursor-index cursor)
                              (cursor-revision cursor)))))
  (buffer nil :read-only t)
  (piece-table nil :read-only t)
  piece
  (byte-offset 0 :type idx)
  (lineno nil :type (or null idx))
  (index 0 :type idx)
  (lock nil :type t) ; used to safeguard COPY-CURSOR
  (revision 0 :type idx)
  (tracked-p nil :type boolean) ; if T, we can always access
  (static-p nil :type boolean))

(eval-when (:compile-toplevel :load-toplevel)
  (pushnew 'cursor buf::*cursor-types*))

(defun signal-bad-cursor-index (cursor index)
  (let ((pt (cursor-piece-table cursor)))
    (error 'conditions:vico-bad-index
           :buffer pt
           :index index
           :bounds (list* 0 (pt-size pt)))))

(defun signal-bad-cursor-line (cursor overrun)
  (error 'vico-piece-table-bad-line-number
         :buffer (cursor-piece-table cursor)
         :overrun overrun))

(defun make-cursor/unlocked (pt index track static track-lineno-p)
  (let ((%pt (slot-value pt 'piece-table)))
    (when (> index (pt-size %pt))
      (error 'conditions:vico-bad-index
             :buffer pt
             :bad-index index
             :bounds (list* 0 (pt-size %pt))))
    (loop :initially (when (= index (pt-size %pt)) ;off-end
                       (let* ((end-piece (piece-prev (pt-sentinel-end %pt)))
                              (cursor (make-cursor :buffer pt
                                                   :piece-table %pt
                                                   :piece end-piece
                                                   :byte-offset (piece-size end-piece)
                                                   :index index
                                                   :revision revision
                                                   :tracked-p track
                                                   :static-p static)))
                         (when track
                           (vector-push-extend cursor (pt-tracked-cursors
                                                       (cursor-piece-table cursor))))
                         (when track-lineno-p
                           (setf (cursor-lineno cursor) (line-at cursor)))
                         (return cursor)))
          :with revision = (pt-revision %pt)
          :with piece-index :of-type idx
          :for piece = (piece-next (pt-sentinel-start %pt)) :then (piece-next piece)
          :while (piece-next piece)
          :when (<= piece-index index (+ piece-index (1- (piece-size piece))))
            :return (let ((cursor (make-cursor :buffer pt
                                               :piece-table %pt
                                               :piece piece
                                               :byte-offset (- index piece-index)
                                               :index index
                                               :revision revision
                                               :tracked-p track
                                               :static-p static)))
                      (when track
                        (vector-push-extend cursor (pt-tracked-cursors
                                                    (cursor-piece-table cursor))))
                      (when track-lineno-p
                        (setf (cursor-lineno cursor) (line-at cursor)))
                      cursor)
          :do (incf piece-index (piece-size piece)))))

(defmethod buf:make-cursor ((pt piece-table-buffer) index &key track static track-lineno-p)
  (with-pt-lock ((slot-value pt 'piece-table))
    (make-cursor/unlocked pt index track static track-lineno-p)))

(defun copy-cursor (cursor)
  (let ((copy (with-cursor-lock (cursor) (%copy-cursor cursor))))
    (setf (cursor-lock copy) nil
          (cursor-tracked-p copy) nil)
    copy))

(defun copy-cursor/unlocked (cursor)
  (let ((copy (%copy-cursor cursor)))
    (setf (cursor-lock copy) nil
          (cursor-tracked-p copy) nil)
    copy))

(defmethod buf:copy-cursor ((cursor cursor))
  (copy-cursor cursor))

(declaim (inline cursor= cursor/= cursor< cursor> cursor<= cursor>=))
(macrolet ((def-cursor-arith (op)
             (let ((name (symbolicate "CURSOR" (string op))))
               `(progn
                  (defun ,name (cursor1 cursor2)
                    (,op (cursor-index cursor1) (cursor-index cursor2)))
                  (defmethod ,(find-symbol (string name) :vico-core.buffer)
                      ((cursor1 cursor) (cursor2 cursor))
                    (,name cursor1 cursor2))))))
  (def-cursor-arith =)
  (def-cursor-arith /=)
  (def-cursor-arith <)
  (def-cursor-arith >)
  (def-cursor-arith <=)
  (def-cursor-arith >=)
  (def-cursor-arith +)
  (def-cursor-arith -))

(defun %blit-cursor (dest src)
  "CAREFUL: very dangerous. Only use if at least one cursor is private to your thread.
Any cursor which is not private must be locked. They must correspond to the same buffer."
  (setf (cursor-byte-offset dest) (cursor-byte-offset src)
        (cursor-piece dest) (cursor-piece src)
        (cursor-index dest) (cursor-index src)
        (cursor-lineno dest) (cursor-lineno src))
  dest)

(defmethod buf:cursor-buffer ((cursor cursor)) ;unlocked
  (cursor-buffer cursor))

(declaim (inline get-revision check-revision))
(defun get-revision (pt)
  (let ((pre-revision (pt-revision pt)))
    ;; revision read occurs before critical code following GET-REVISION
    #+sbcl (sb-thread:barrier (:read))
    (logand pre-revision (lognot 1)))) ; odd counts reduced by 1 to auto-invalidate

(defun check-revision (pt cursor pre-revision)
  #+sbcl (sb-thread:barrier (:read))
  ;; revision read occurs after critical code preceding CHECK-REVISION
  (and (= (pt-revision pt) pre-revision)
       (= (cursor-revision cursor) pre-revision)))

(defmethod buf:cursor-static-p ((cursor cursor))
  (cursor-static-p cursor))

(defmethod (setf buf:cursor-static-p) (new-value (cursor cursor))
  (setf (cursor-static-p cursor) new-value))

(defmethod buf:cursor-tracked-p ((cursor cursor))
  (cursor-tracked-p cursor))

(defmethod (setf buf:cursor-tracked-p) (new-value (cursor cursor))
  (setf (cursor-tracked-p cursor) new-value)
  (let ((tracked-cursors (pt-tracked-cursors (cursor-piece-table cursor))))
    (if new-value
        (vector-push-extend cursor tracked-cursors)
        (let ((position (position cursor tracked-cursors)))
          (setf (aref tracked-cursors position)
                (aref tracked-cursors (decf (fill-pointer tracked-cursors))))))))

;; readers

(declaim (inline index-at))
(defun index-at (cursor)
  (cursor-index cursor))

(defmethod buf:index-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (index (index-at cursor)))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    index))

(declaim (ftype (function (t) idx) line-at))
(defun line-at (cursor)
  (declare (optimize speed (safety 0)))
  (or (cursor-lineno cursor)
      (loop :with line :of-type idx = (count-lfs (piece-data (cursor-piece cursor))
                                                 0
                                                 (cursor-byte-offset cursor))
            :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
            :while (and piece (piece-prev piece))
            :do (incf line (count-lfs (piece-data piece) 0 (piece-size piece)))
            :finally (return (1+ line)))))

(defmethod buf:line-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (line (line-at cursor)))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    line))

(declaim (inline byte-at))
(defun byte-at (cursor)
  (declare (optimize speed (safety 0)))
  (ffi:mem-ref (inc-ptr (piece-data (cursor-piece cursor)) (cursor-byte-offset cursor))
               :unsigned-char))

(defmethod buf:byte-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (byte (byte-at cursor)))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    byte))


(declaim (inline utf8-char-at))
(defun utf8-char-at (octets max)
  (declare (optimize speed (safety 0))
           (type ffi:foreign-pointer octets)
           (type idx max)) ; safe because we check against out of bounds accesses
  (let* ((leading-byte (ffi:mem-ref octets :unsigned-char))
         (char-byte-size (cond ((< leading-byte #x80) 1)
                               ((< leading-byte #xE0) 2)
                               ((< leading-byte #xF0) 3)
                               ((< leading-byte #xF8) 4)
                               (t (return-from utf8-char-at #.(code-char #xfffd)))))
         (codepoint (logand leading-byte (ecase char-byte-size
                                           (1 #xFF)
                                           (2 #x1F)
                                           (3 #x0F)
                                           (4 #x07)))))
    (declare (type (integer 0 #x10ffff) codepoint))
    (loop :initially (or (<= char-byte-size max) (return #.(code-char #xfffd)))
          :for i :from 1 :below char-byte-size
          :do (setf codepoint (logior (ash codepoint 6)
                                      (logand (ffi:mem-ref octets :unsigned-char i) #x3F)))
          :finally (return (code-char codepoint)))))

(defun char-at (cursor)
  (let ((piece (cursor-piece cursor)))
    (utf8-char-at (inc-ptr (piece-data piece) (cursor-byte-offset cursor))
                  (- (piece-size piece) (cursor-byte-offset cursor)))))

(defmethod buf:char-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (char (char-at cursor)))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (if (and (< (cursor-index cursor) (pt-size (cursor-piece-table cursor)))
             char)
        char
        (error 'conditions:vico-bad-index
               :buffer pt
               :index (cursor-index cursor)
               :bounds (list* 0 (1- (pt-size pt)))))))

;; movement

;; note to self: the IF branches are not necessary, but are possibly clearer and more
;; efficient (likely branch, less+) - reflects byte-offset = 0 in subsequent iterations

(defun cursor-next (cursor count)
  (declare (optimize speed (safety 0))
           (type idx count))
  #-sbcl (check-type count idx)
  (if (< (+ (cursor-byte-offset cursor) count) (piece-size (cursor-piece cursor)))
      (progn
        (when (cursor-lineno cursor)
          (incf (cursor-lineno cursor)
                (count-lfs (piece-data (cursor-piece cursor))
                           (cursor-byte-offset cursor)
                           (+ (cursor-byte-offset cursor) count))))
        (incf (cursor-byte-offset cursor) count))
      (loop :initially (let ((piece (cursor-piece cursor)))
                         (or (piece-next (cursor-piece cursor)) ; empty
                             (return-from cursor-next count))
                         (decf left (- (piece-size piece) (cursor-byte-offset cursor)))
                         (when (cursor-lineno cursor)
                           (incf (cursor-lineno cursor)
                                 (count-lfs (piece-data piece)
                                            (cursor-byte-offset cursor)
                                            (piece-size piece)))))
            :with left :of-type idx = count
            :for piece = (piece-next (cursor-piece cursor)) :then (piece-next piece)
            :while (and (>= left (piece-size piece)) (piece-next piece))
            :do (decf left (piece-size piece))
                (when (cursor-lineno cursor)
                  (incf (cursor-lineno cursor)
                        (count-lfs (piece-data piece) 0 (piece-size piece))))
            :finally (cond ((piece-next piece)
                            (when (cursor-lineno cursor)
                              (incf (cursor-lineno cursor)
                                    (count-lfs (piece-data piece) 0 left)))
                            (setf (cursor-piece cursor) piece
                                  (cursor-byte-offset cursor) left))
                           ((zerop left) ;end-of-buffer
                            (setf (cursor-piece cursor) (piece-prev piece)
                                  (cursor-byte-offset cursor) (piece-size
                                                               (cursor-piece cursor))))
                           (t (return-from cursor-next left)))))
  (incf (cursor-index cursor) count)
  nil)

(defmethod buf:cursor-next ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-next cursor count))))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (signal-bad-cursor-index cursor (+ (pt-size pt) result))))
  cursor)

;; same as above, but byte-offset = piece-size in subsequent iterations

(defun cursor-prev (cursor count)
  (declare (optimize speed (safety 0))
           (type idx count))
  #-sbcl (check-type count idx)
  (if (>= (cursor-byte-offset cursor) count)
      (progn
        (when (cursor-lineno cursor)
          (decf (cursor-lineno cursor)
                (count-lfs (piece-data (cursor-piece cursor))
                           (- (cursor-byte-offset cursor) count)
                           (cursor-byte-offset cursor))))
        (decf (cursor-byte-offset cursor) count))
      (loop :initially (or (piece-prev (cursor-piece cursor)) ; empty
                           (return-from cursor-prev left))
                       (decf left (cursor-byte-offset cursor))
                       (when (cursor-lineno cursor)
                         (decf (cursor-lineno cursor)
                               (count-lfs (piece-data (cursor-piece cursor))
                                          0
                                          (cursor-byte-offset cursor))))
            :with left :of-type idx = count
            :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
            :while (and (> left (piece-size piece)) (piece-prev piece))
            :do (decf left (piece-size piece))
                (when (cursor-lineno cursor)
                  (decf (cursor-lineno cursor)
                        (count-lfs (piece-data piece) 0 (piece-size piece))))
            :finally (if (piece-prev piece)
                         (progn
                           (when (cursor-lineno cursor)
                             (decf (cursor-lineno cursor)
                                   (count-lfs (piece-data piece)
                                              (- (piece-size piece) left)
                                              (piece-size piece))))
                           (setf (cursor-piece cursor) piece
                                 (cursor-byte-offset cursor) (- (piece-size piece) left)))
                         (return-from cursor-prev left))))
  (decf (cursor-index cursor) count)
  nil)

(defmethod buf:cursor-prev ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-prev cursor count))))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (signal-bad-cursor-index cursor (- result))))
  cursor)

;;  0     1         2      3 4
;; .\nx...\n....|...\n.....\nx.|...
;; 3  ^                         ^

(defun cursor-next-line (cursor count)
  (declare (optimize speed)
           (type idx count))
  #-sbcl (check-type count idx)
  (loop :with remaining-count = count
        :with copy = (%copy-cursor cursor)
        :for piece = (cursor-piece copy) :then (piece-next piece)
        :for start = (inc-ptr (piece-data piece) (cursor-byte-offset copy))
        :do (loop :with piece-end :of-type idx = (+ (the idx (ffi:pointer-address
                                                              (piece-data piece)))
                                                    (piece-size piece))
                  :for left :of-type idx = (- piece-end
                                              (the idx (ffi:pointer-address start)))
                  :while (and (plusp remaining-count) (plusp left))
                  :do (let ((found (ffi:pointer-address
                                    (ffi:foreign-funcall "memchr"
                                                         :pointer start
                                                         :int #.(char-code #\newline)
                                                         size-t left
                                                         :pointer))))
                        (declare (type idx found))
                        (when (zerop found) ; NULL
                          (cursor-next copy left)
                          (return))
                        (let ((inc (1+ (- found (the idx (ffi:pointer-address start))))))
                          (cursor-next copy inc)
                          (setf start (inc-ptr start inc))))
                      (decf remaining-count))
        :while (and (plusp remaining-count) (piece-next piece)) ;not sentinel
        :finally (or (piece-next piece)
                     (return-from cursor-next-line remaining-count))
                 (%blit-cursor cursor copy)))

(defmethod buf:cursor-next-line ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-next-line cursor count))))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (signal-bad-cursor-line cursor result)))
  cursor)

(defun cursor-prev-line (cursor count)
  (declare (optimize speed)
           (type idx count))
  #-sbcl (check-type count idx)
  (loop :with needed-count = (1+ count) ; jump backwards count+1, step forward once
        :with copy = (%copy-cursor cursor)
        :for piece = (cursor-piece copy) :then (piece-prev piece)
        :for start-offset :of-type idx = (cursor-byte-offset copy)
          :then (piece-size piece)
        :do (loop :with found :of-type idx
                  :with start-ptr = (piece-data piece)
                  :while (and (plusp needed-count) (plusp start-offset))
                  :do (setf found
                            (ffi:pointer-address
                             (ffi:foreign-funcall "memrchr"
                                                  :pointer start-ptr
                                                  :int #.(char-code #\newline)
                                                  size-t start-offset
                                                  :pointer)))
                      (when (zerop found) ; NULL, move back a piece
                        (cursor-prev copy start-offset)
                        (return))
                      (let ((dec (- (the idx (+ (the idx (ffi:pointer-address start-ptr))
                                                start-offset))
                                    found)))
                        (cursor-prev copy dec))
                      (setf start-offset (- found
                                            (the idx (ffi:pointer-address start-ptr))))
                      (decf needed-count))
        :while (and (plusp needed-count) (piece-prev piece)) ; not sentinel
        :finally (cond ((piece-prev piece)
                        (cursor-next copy 1)
                        (%blit-cursor cursor copy))
                       ((= needed-count 1) ; start of buffer
                        (setf (cursor-piece copy) (piece-next piece)
                              (cursor-byte-offset copy) 0
                              (cursor-index copy) 0
                              (cursor-lineno copy) 1)
                        (%blit-cursor cursor copy))
                       (t (return-from cursor-prev-line (1- needed-count))))))

(defmethod buf:cursor-prev-line ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-prev-line cursor count))))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (signal-bad-cursor-line cursor (- result))))
  cursor)

;; PR for babel to have leading-byte test - encodings

(defun cursor-next-char (cursor count)
  (declare (optimize speed)
           (type idx count))
  (loop :repeat count
        :with byte-offset :of-type idx
        :do (loop
              (when (cursor-next cursor 1)
                (let ((overrun (1+ (cursor-index cursor))))
                  (cursor-prev cursor byte-offset)
                  (return-from cursor-next-char overrun)))
              (incf byte-offset)
              (let ((byte (byte-at cursor)))
                (when (/= (logand byte #xC0) #x80) ; leading utf-8 byte
                  (return))))))

(defmethod buf:cursor-next-char ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-next-char cursor count))))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (signal-bad-cursor-index cursor result)))
  cursor)

(defun cursor-prev-char (cursor count)
  (declare (optimize speed)
           (type idx count))
  (loop :repeat count
        :with byte-offset :of-type idx
        :do (loop
              (when (cursor-prev cursor 1)
                (cursor-next cursor byte-offset)
                (return-from cursor-prev-char -1))
              (incf byte-offset)
              (let ((byte (byte-at cursor)))
                (when (/= (logand byte #xC0) #x80)
                  (return))))))

(defmethod buf:cursor-prev-char ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-prev-char cursor count))))
    (or (check-revision pt cursor pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (signal-bad-cursor-index cursor result)))
  cursor)

(defmethod buf:cursor-find-next ((cursor cursor) char)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (result (loop :until (or (= (cursor-index copy) (pt-size pt))
                                    (char= (char-at copy) char))
                         :do (when (cursor-next-char copy 1)
                               (return t))
                         :finally (%blit-cursor cursor copy))))
      (or (check-revision pt cursor pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      (if result
          nil
          (values char cursor)))))

(defmethod buf:cursor-find-prev ((cursor cursor) char)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (result (loop :do (when (cursor-prev-char copy 1)
                               (return t))
                         :until (or (= (cursor-index copy) (pt-size pt))
                                    (char= (char-at copy) char))
                         :finally (%blit-cursor cursor copy))))
      (or (check-revision pt cursor pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      (if result
          nil
          (values char cursor)))))

;; start anchors match the current cursor position when searching forward
;; end anchors match the current position when searching backwards
;; (they turn into start anchors) use multiline mode

(defun reverse-regex (regex)
  (labels ((recur (o)
             (cond ((atom o)
                    (case o
                      (:end-anchor :start-anchor)
                      (:start-anchor :end-anchor)
                      (:positive-lookbehind :positive-lookahead)
                      (:negative-lookbehind :negative-lookahead)
                      (:positive-lookahead :positive-lookbehind)
                      (:negative-lookahead :negative-lookbehind)
                      (otherwise (if (stringp o)
                                     (nreverse o)
                                     o))))
                   ((eq (first o) :sequence)
                    (list* :sequence (nreverse (recur (cdr o)))))
                   (t
                    (map 'list #'recur o)))))
    (recur (ppcre:parse-string regex))))

;; TODO BMH matchers break on random binary files
;; TODO as it is now, this hack will not work properly for multibyte searches matching
;; (greedily) at buffer boundaries, where we cut off the search (it's wasteful to
;; compute codepoint bounding indices).
;; But this is fine for now since I'll be rewriting the buffer in rust

(defmethod buf:cursor-search-next ((cursor cursor) string &optional max-chars)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (char-offset 0)
           (result (block nil
                     (setf (cursor-lineno copy) nil)
                     (let* ((ppcre:*use-bmh-matchers* t)
                            (fn (lambda (buffer index)
                                  (declare (ignore buffer))
                                  (let* ((delta (- index char-offset)))
                                    (when (and max-chars
                                               (>= (+ char-offset delta) max-chars))
                                      (return))
                                    (when (if (plusp delta)
                                              (cursor-next-char copy delta)
                                              (cursor-prev-char copy (- delta)))
                                      (return))
                                    (incf char-offset delta))
                                  (char-at copy))))
                       ;; :end prevents call to CL:LENGTH, pt-size >= chars
                       (multiple-value-bind (start end)
                           (ppcre:scan string pt :accessor fn :end (pt-size pt))
                         (when start
                           (cursor-next-char cursor start)
                           (- end start)))))))
      (or (check-revision pt cursor pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      result)))

(defmethod buf:cursor-search-prev ((cursor cursor) string &optional max-chars)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (char-backwards-offset 0)
           (result (block nil
                     (setf (cursor-lineno copy) nil)
                     (let ((ppcre:*use-bmh-matchers* t)
                           (fn (lambda (buffer index) ; provide a reversed stream
                                 (declare (ignore buffer))
                                 (let ((delta (- index char-backwards-offset)))
                                   (when (and max-chars
                                              (> (+ char-backwards-offset delta) max-chars))
                                     (return))
                                   (when (if (plusp delta)
                                             (cursor-prev-char copy delta)
                                             (cursor-next-char copy (- delta)))
                                     (return))
                                   (incf char-backwards-offset delta))
                                 (char-at copy))))
                       (multiple-value-bind (start end)
                           (ppcre:scan (if (stringp string)
                                           (reverse-regex string)
                                           string)
                                       pt :accessor fn
                                       :end (1+ (cursor-index cursor)))
                         (when start
                           (cursor-prev-char cursor (1- end))
                           (- end start)))))))
      (or (check-revision pt cursor pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      result)))

;; modification

(defstruct span
  start
  end
  (size 0 :type idx))

(defun span-swap (pt old new)
  (cond ((and (zerop (span-size old)) (zerop (span-size new))))
        ((zerop (span-size old)) ; insert new
         (setf (piece-next (piece-prev (span-start new))) (span-start new)
               (piece-prev (piece-next (span-end new))) (span-end new)))
        ((zerop (span-size new)) ; delete old
         (setf (piece-next (piece-prev (span-start old))) (piece-next (span-end old))
               (piece-prev (piece-next (span-end old))) (piece-prev (span-start old))))
        (t
         (setf (piece-next (piece-prev (span-start old))) (span-start new)
               (piece-prev (piece-next (span-end old))) (span-end new))))
  (decf (pt-size pt) (span-size old))
  (incf (pt-size pt) (span-size new)))

(defstruct edit
  old
  new
  cursors)

(defstruct group-edit
  )

(defstruct cursor-edit
  (old nil :read-only t)
  (new nil :read-only t)
  (real nil :read-only t))

(defun map-piece-data (pt fn start count &optional start-piece start-offset)
  (declare (optimize debug))
  (or start-piece start-offset
      (multiple-value-setq (start-piece start-offset)
        (loop :for left = start
              :for piece = (piece-next (pt-sentinel-start pt))
                :then (piece-next piece)
              :while (> left (piece-size piece))
              :do (decf left (piece-size piece))
              :finally (return (values piece left)))))
  (loop :for piece = start-piece :then (piece-next piece)
        :for offset = start-offset :then 0
        :while (and (> (+ offset count) (piece-size piece))
                    (piece-next piece))
        :do (funcall fn
                     (inc-ptr (piece-data piece) offset)
                     (piece-size piece))
            (decf count (- (piece-size piece) offset))
        :finally (funcall fn
                          (inc-ptr (piece-data piece) offset)
                          count)))

(defmethod buf:write-to-octet-stream ((pt piece-table-buffer) stream
                                      &key (start 0) (end (pt-size pt)))
  (let ((pt (slot-value pt 'piece-table)))
    (or (<= 0 start (pt-size pt))
        (error 'conditions:vico-bad-index
               :index start
               :bounds (list* 0 (pt-size pt))))
    (or (<= start end (pt-size pt))
        (error 'conditions:vico-bad-index
               :index end
               :bounds (list* start (pt-size pt))))
    (let ((count (- end start)))
      (map-piece-data pt #'(lambda (ptr len)
                             (write-sequence
                              (ffi:foreign-array-to-lisp ptr (list :array :unsigned-char
                                                                   len))
                              stream))
                      start count)
      (finish-output stream))))

;; boundary case
;;        v <- cursor
;; [prev]-[cursor-piece]-[next]
;;              v <- cursor, unchanged
;; [prev]-[new]-[cursor-piece]-[next]

;; middle of piece
;;                      v <- cursor
;; [prev]-[      cursor-piece     ]-[next]
;;                                        v <- cursor
;; [prev]-[left-split]-[new-cursor-piece]-[right-split]-[next]

(defun data-buffer-append (pt string)
  "Inserts STRING into PT's first data buffer, or makes a new one if capacity is full.
Returns a pointer and byte length of STRING encoded in PT's encoding."
  (let ((strlen (babel:string-size-in-octets string))
        (buf (first (pt-data-buffers pt))))
    (if (or (null buf) (> (+ strlen (data-buffer-size buf)) (data-buffer-capacity buf)))
        (let* ((new-capacity (max strlen +min-data-buffer-size+)) ; v harmless null for now
               (new-char* (ffi:foreign-alloc :unsigned-char :count new-capacity))
               (new-buffer (make-alloc-buffer :size strlen
                                              :capacity new-capacity
                                              :data new-char*)))
          (ffi:lisp-string-to-foreign string new-char*
                                      (1+ strlen)) ;no. bytes including null

          (push new-buffer (pt-data-buffers pt))
          (values new-char* strlen nil))
        (let ((new-char* (ffi:lisp-string-to-foreign string (data-buffer-data buf)
                                                     (1+ strlen) ;no. bytes including null
                                                     :offset (data-buffer-size buf))))
          (incf (data-buffer-size buf) strlen)
          (values new-char* strlen t)))))

(defun data-buffer-append-bytes (pt octets)
  (let ((strlen (length octets))
        (buf (first (pt-data-buffers pt))))
    (if (or (null buf) (> (+ strlen (data-buffer-size buf)) (data-buffer-capacity buf)))
        (let* ((new-capacity (max strlen +min-data-buffer-size+)) ; v harmless null for now
               (new-char* (ffi:foreign-alloc :unsigned-char :count new-capacity
                                                            :initial-contents octets))
               (new-buffer (make-alloc-buffer :size strlen
                                              :capacity new-capacity
                                              :data new-char*)))

          (push new-buffer (pt-data-buffers pt))
          (values new-char* strlen nil))
        (let ((new-char* (ffi:lisp-array-to-foreign octets
                                                    (inc-ptr (data-buffer-data buf)
                                                             (data-buffer-size buf))
                                                    (list :array :unsigned-char strlen))))
          (incf (data-buffer-size buf) strlen)
          (values new-char* strlen t)))))

(defun insert-at (cursor new-ptr strlen appendp)
  (let* ((pt (cursor-piece-table cursor))
         (tracked-cursors (pt-tracked-cursors pt))
         (stack (pt-undo-stack pt))
         (changed-cursors (make-array 0 :fill-pointer t :adjustable t))
         (static (cursor-static-p cursor))
         (piece (cursor-piece cursor))
         (prev (piece-prev piece))
         old-span new-span)
    (flet ((add-cursor-change (old new)
             (vector-push-extend (make-cursor-edit :old old
                                                   :new (copy-cursor/unlocked new)
                                                   :real new)
                                 changed-cursors)))
      (declare (dynamic-extent #'add-cursor-change))
      (macrolet ((with-cursor-updating ((cursor) &body body)
                   `(let ((old (copy-cursor/unlocked ,cursor))) ; n.b. captures OLD
                      ,@body
                      (add-cursor-change old ,cursor))))
        (cond ((= (cursor-byte-offset cursor) (piece-size piece)) ; off-end
               (if (and (eq piece (pt-end-cache pt)) appendp)
                   (let ((new-piece (make-piece :prev prev :next (piece-next piece)
                                                :data (piece-data piece)
                                                :size (+ (piece-size piece)
                                                         strlen))))
                     (setf (pt-end-cache pt) new-piece)
                     ;; set non-static off-end cursors to new size
                     (map () #'(lambda (tcursor)
                                 (when (eq (cursor-piece tcursor) piece)
                                   (with-cursor-updating (tcursor)
                                     (setf (cursor-piece tcursor) new-piece)
                                     (when (and (cursor= tcursor cursor)
                                                (not (cursor-static-p tcursor)))
                                       (setf (cursor-byte-offset tcursor)
                                             (piece-size new-piece))))))
                          tracked-cursors)
                     (or static
                         (cursor-tracked-p cursor)
                         (with-cursor-updating (cursor)
                           (setf (cursor-byte-offset cursor) (piece-size new-piece))))
                     (setf old-span (make-span :start piece :end piece
                                               :size (piece-size piece))
                           new-span (make-span :start new-piece :end new-piece
                                               :size (piece-size new-piece))))
                   ;; new, record this as an edit
                   (let ((new-piece (make-piece :prev piece :next (piece-next piece)
                                                :data new-ptr
                                                :size strlen)))
                     (setf (pt-end-cache pt) new-piece)
                     ;; update cursors
                     (map () #'(lambda (tcursor)
                                 (when (cursor= tcursor cursor)
                                   (with-cursor-updating (tcursor)
                                     (setf (cursor-piece tcursor) new-piece)
                                     (setf (cursor-byte-offset tcursor)
                                           (if (cursor-static-p tcursor)
                                               0
                                               (piece-size new-piece))))))
                          tracked-cursors)
                     (or (cursor-tracked-p cursor)
                         (with-cursor-updating (cursor)
                           (setf (cursor-piece cursor) new-piece
                                 (cursor-byte-offset cursor)
                                 (if static
                                     0
                                     (piece-size new-piece)))))
                     ;; record edit
                     (setf old-span (make-span)
                           new-span (make-span :start new-piece :end new-piece
                                               :size strlen)))))
              ((zerop (cursor-byte-offset cursor)) ; boundary case
               (if (and (eq prev (pt-end-cache pt)) appendp)
                   (let ((new-prev (make-piece :prev (piece-prev prev)
                                               :next piece
                                               :data (piece-data prev)
                                               :size (+ (piece-size prev)
                                                        strlen))))
                     (setf (pt-end-cache pt) new-prev)
                     ;; update cursors
                     (map () #'(lambda (tcursor)
                                 (cond ((eq (cursor-piece tcursor) prev)
                                        (with-cursor-updating (tcursor)
                                          (setf (cursor-piece tcursor) new-prev)))
                                       ((and (cursor= tcursor cursor)
                                             (cursor-static-p tcursor))
                                        (with-cursor-updating (tcursor)
                                          (setf (cursor-piece tcursor) new-prev
                                                (cursor-byte-offset tcursor)
                                                (piece-size prev))))))
                          tracked-cursors)
                     (when (and static (not (cursor-tracked-p cursor)))
                       (with-cursor-updating (cursor)
                         (setf (cursor-piece cursor) new-prev
                               (cursor-byte-offset cursor) (piece-size prev))))
                     (setf old-span (make-span :start prev :end prev
                                               :size (piece-size prev))
                           new-span (make-span :start new-prev :end new-prev
                                               :size (piece-size new-prev))))
                   ;; new
                   (let ((new-piece (make-piece :prev prev :next piece
                                                :data new-ptr
                                                :size strlen)))
                     (setf (pt-end-cache pt) new-piece)
                     ;; update cursors
                     (map () #'(lambda (tcursor)
                                 (when (and (cursor= tcursor cursor)
                                            (cursor-static-p tcursor))
                                   (with-cursor-updating (tcursor)
                                     ;; we know offset = 0 already
                                     (setf (cursor-piece tcursor) new-piece))))
                          tracked-cursors)
                     (when (and static (not (cursor-tracked-p cursor)))
                       (with-cursor-updating (cursor)
                         (setf (cursor-piece cursor) new-piece)))
                     ;; record edit
                     (setf old-span (make-span)
                           new-span (make-span :start new-piece :end new-piece
                                               :size strlen)))))
              (t ; general case - insertion in middle of piece
               (let* ((next (piece-next piece))
                      (old-cursor-byte-offset (cursor-byte-offset cursor))
                      (new-piece (make-piece :data new-ptr :size strlen))
                      (left-split
                        (make-piece :prev prev :next new-piece
                                    :data (piece-data piece)
                                    :size (cursor-byte-offset cursor)))
                      (right-split
                        (make-piece :prev new-piece :next next
                                    :data (inc-ptr (piece-data piece)
                                                   (cursor-byte-offset cursor))
                                    :size (- (piece-size piece)
                                             (cursor-byte-offset cursor)))))
                 (setf (pt-end-cache pt) new-piece)
                 ;; fix cursors
                 (map () #'(lambda (tcursor)
                             (when (eq (cursor-piece tcursor) piece)
                               (with-cursor-updating (tcursor)
                                 (cond ((> (cursor-byte-offset tcursor)
                                           old-cursor-byte-offset)
                                        (setf (cursor-piece tcursor) right-split)
                                        (decf (cursor-byte-offset tcursor)
                                              old-cursor-byte-offset))
                                       ((= (cursor-byte-offset tcursor)
                                           old-cursor-byte-offset)
                                        (setf (cursor-piece tcursor)
                                              (if (cursor-static-p tcursor)
                                                  new-piece
                                                  right-split))
                                        (setf (cursor-byte-offset tcursor) 0))
                                       (t ; < old-cursor-byte-offset
                                        (setf (cursor-piece tcursor) left-split))))))
                      tracked-cursors)
                 (or (cursor-tracked-p cursor)
                     (with-cursor-updating (cursor)
                       (setf (cursor-piece cursor) (if static new-piece right-split)
                             (cursor-byte-offset cursor) 0)))
                 (setf (piece-prev new-piece) left-split
                       (piece-next new-piece) right-split)
                 ;; record edit
                 (setf old-span (make-span :start piece
                                           :end piece
                                           :size (piece-size piece))
                       new-span (make-span :start left-split
                                           :end right-split
                                           :size (+ (piece-size left-split)
                                                    (piece-size new-piece)
                                                    (piece-size right-split)))))))
        (let ((lfs (count-lfs new-ptr 0 strlen))
              (index (cursor-index cursor)))
          (map () #'(lambda (tcursor)
                      (when (or (and (= (cursor-index tcursor) index)
                                     (not (cursor-static-p tcursor)))
                                (> (cursor-index tcursor) index))
                        (with-cursor-updating (tcursor)
                          (incf (cursor-index tcursor) strlen)
                          (when (cursor-lineno tcursor)
                            (incf (cursor-lineno tcursor) lfs)))))
               tracked-cursors)
          (or static
              (cursor-tracked-p cursor) ; handled above
              (progn
                (with-cursor-updating (cursor)
                  (incf (cursor-index cursor) strlen)
                  (when (cursor-lineno cursor)
                    (incf (cursor-lineno cursor) lfs))))))
        (span-swap pt old-span new-span)
        (setf (fill-pointer stack) (pt-undo-position pt))
        (vector-push-extend (make-edit :old old-span :new new-span
                                       :cursors changed-cursors)
                            stack)
        (incf (pt-undo-position pt))))))

(defmethod buf:insert-at ((cursor cursor) string)
  (unless (zerop (length string))
    (let* ((pt (cursor-piece-table cursor))
           (tracked-cursors (pt-tracked-cursors pt)))
      (atomics:atomic-incf (pt-revision pt))
      #+sbcl (sb-thread:barrier (:write))
      (with-pt-lock (pt)
        (map () #'(lambda (tcursor)
                    (lock-spinlock (cursor-lock tcursor)))
             tracked-cursors)
        (or (cursor-tracked-p cursor) (lock-spinlock (cursor-lock cursor)))
        (multiple-value-bind (new-ptr strlen appendp)
            (data-buffer-append pt string)
          (insert-at cursor new-ptr strlen appendp))
        ;; cleanup
        (map () #'(lambda (tcursor)
                    (incf (cursor-revision tcursor) 2)
                    (unlock-spinlock (cursor-lock tcursor)))
             tracked-cursors)
        (or (cursor-tracked-p cursor)
            (progn
              (incf (cursor-revision cursor) 2)
              (unlock-spinlock (cursor-lock cursor))))
        #+sbcl (sb-thread:barrier (:write))
        (atomics:atomic-incf (pt-revision pt)))
      pt)))

(defun delete-multiple (pt cursor count)
  "Deletion spanning multiple pieces"
  (let* ((tracked-cursors (pt-tracked-cursors pt))
         (stack (pt-undo-stack pt))
         (changed-cursors (make-array 0 :fill-pointer t :adjustable t))
         (piece (cursor-piece cursor))
         (prev (piece-prev piece))
         (old-byte-offset (cursor-byte-offset cursor))
         new-piece new-last
         old-span new-span)
    (multiple-value-bind (last last-offset)
        (loop :initially (decf remaining (- (piece-size piece) old-byte-offset))
              :for last = (piece-next piece) :then (piece-next last)
              :with remaining :of-type idx = count
              :while (and (piece-next last) (> remaining (piece-size last)))
              :do (when (eq (pt-end-cache pt) last)
                    (setf (pt-end-cache pt) nil))
                  (decf remaining (piece-size last))
              :finally (or (piece-next last) (return-from delete-multiple)) ; overrun
                       (return (values last remaining)))
      (flet ((add-cursor-change (old new)
               (vector-push-extend (make-cursor-edit
                                    :old old
                                    :new (copy-cursor/unlocked new)
                                    :real new)
                                   changed-cursors)))
        (declare (dynamic-extent #'add-cursor-change))
        (macrolet ((with-cursor-updating ((cursor) &body body)
                     `(let ((old (copy-cursor/unlocked ,cursor)))
                        ,@body
                        (add-cursor-change old ,cursor))))
          (map () #'(lambda (tcursor) ; will never update CURSOR
                      (cond ((< (cursor-index cursor)
                                (cursor-index tcursor)
                                (+ (cursor-index cursor) count))
                             (with-cursor-updating (tcursor)
                               (setf (cursor-index tcursor) (cursor-index cursor))))
                            ((>= (cursor-index tcursor) (+ (cursor-index cursor) count))
                             (with-cursor-updating (tcursor)
                               (decf (cursor-index tcursor) count)))))
               tracked-cursors)
          ;; update tracked line numbers
          (loop :initially (map () #'(lambda (tcursor)
                                       (when (and (cursor-lineno tcursor)
                                                  (eq (cursor-piece tcursor) piece)
                                                  ;; can't be equal: not end
                                                  (< (cursor-byte-offset cursor)
                                                     (cursor-byte-offset tcursor)
                                                     (piece-size piece)))
                                         (with-cursor-updating (tcursor)
                                           (decf (cursor-lineno tcursor)
                                                 (count-lfs (piece-data piece)
                                                            (cursor-byte-offset cursor)
                                                            (cursor-byte-offset tcursor))))))
                                tracked-cursors)
                :with lfs = (count-lfs (piece-data piece)
                                       (cursor-byte-offset cursor)
                                       (piece-size piece))
                :for it = (piece-next piece) :then (piece-next it)
                :until (eq it last)
                :do (map () #'(lambda (tcursor)
                                (when (and (cursor-lineno tcursor)
                                           (eq (cursor-piece tcursor) it))
                                  (with-cursor-updating (tcursor)
                                    (decf (cursor-lineno tcursor)
                                          (+ lfs
                                             (count-lfs (piece-data it)
                                                        0
                                                        (cursor-byte-offset tcursor)))))))
                         tracked-cursors)
                    (incf lfs (count-lfs (piece-data it) 0 (piece-size it)))
                :finally (map () #'(lambda (tcursor)
                                     (when (cursor-lineno tcursor)
                                       (cond ((and (eq (cursor-piece tcursor) last)
                                                   (< (cursor-byte-offset tcursor)
                                                      last-offset))
                                              (with-cursor-updating (tcursor)
                                                (decf (cursor-lineno tcursor)
                                                      (+ lfs
                                                         (count-lfs
                                                          (piece-data it)
                                                          0
                                                          (cursor-byte-offset tcursor))))))
                                             ((>= (cursor-index tcursor)
                                                  (+ (cursor-index cursor) count))
                                              (with-cursor-updating (tcursor)
                                                (decf (cursor-lineno tcursor)
                                                      (+ lfs
                                                         (count-lfs
                                                          (piece-data it)
                                                          0
                                                          (cursor-byte-offset cursor)))))))))
                              tracked-cursors))
          ;;
          (or (zerop old-byte-offset)
              (setf new-piece (make-piece :prev prev
                                          :data (piece-data piece)
                                          :size old-byte-offset)))
          (or (= last-offset (piece-size last))
              (setf new-last (make-piece :next (piece-next last)
                                         :data (inc-ptr (piece-data last) last-offset)
                                         :size (- (piece-size last) last-offset))))
          (when (eq (pt-end-cache pt) piece)
            (setf (pt-end-cache pt) nil))
          (when (eq (pt-end-cache pt) last)
            (setf (pt-end-cache pt) new-last))
          ;; updating cursors
          (let ((last-piece-p (null (piece-next (piece-next last)))))
            (if (or new-last (not last-piece-p))
                (let ((new-last (or new-last (piece-next last)))
                      (new-piece (or new-piece prev)))
                  ;;(vico-core.evloop:log-event :delete-case-multiple-not-end)
                  (map () #'(lambda (tcursor)
                              (cond ((<= (cursor-index cursor)
                                         (cursor-index tcursor)
                                         (+ (cursor-index cursor) count))
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) new-last
                                             (cursor-byte-offset tcursor) 0)))
                                    ((eq (cursor-piece tcursor) piece) ; before range
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) new-piece)))
                                    ((eq (cursor-piece tcursor) last) ; beyond range
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) new-last)
                                       (decf (cursor-byte-offset tcursor) last-offset)))))
                       tracked-cursors)
                  (or (cursor-tracked-p cursor)
                      (with-cursor-updating (cursor)
                        (setf (cursor-piece cursor) new-last
                              (cursor-byte-offset cursor) 0))))
                ;; we're deleting to the end
                (let ((new-piece (or new-piece prev)))
                  ;;(vico-core.evloop:log-event :delete-case-multiple-to-end)
                  (map () #'(lambda (tcursor)
                              (cond ((cursor>= tcursor cursor)
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) new-piece)
                                       (setf (cursor-byte-offset tcursor)
                                             (piece-size new-piece))))
                                    ((eq (cursor-piece tcursor) piece)
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) new-piece)))))
                       tracked-cursors)
                  (or (cursor-tracked-p cursor)
                      (with-cursor-updating (cursor)
                        (setf (cursor-piece cursor) new-piece
                              (cursor-byte-offset cursor) (piece-size new-piece))))))
            ;; relink
            (let ((deleted-pieces-size (+ old-byte-offset count (- (piece-size last)
                                                                   last-offset))))
              (if new-last
                  (if new-piece
                      (setf (piece-next new-piece) new-last
                            (piece-prev new-last) new-piece
                            old-span (make-span :start piece
                                                :end last
                                                :size deleted-pieces-size)
                            new-span (make-span :start new-piece
                                                :end new-last
                                                :size (+ (piece-size new-piece)
                                                         (piece-size new-last))))
                      (setf (piece-prev new-last) prev
                            old-span (make-span :start piece
                                                :end last
                                                :size deleted-pieces-size)
                            new-span (make-span :start new-last
                                                :end new-last
                                                :size (piece-size new-last))))
                  (if new-piece
                      (setf (piece-next new-piece) (piece-next last)
                            old-span (make-span :start piece
                                                :end last
                                                :size deleted-pieces-size)
                            new-span (make-span :start new-piece
                                                :end new-piece
                                                :size (piece-size new-piece)))
                      (setf old-span (make-span :start piece
                                                :end last
                                                :size deleted-pieces-size)
                            new-span (make-span)))))))))
    (span-swap pt old-span new-span)
    (setf (fill-pointer stack) (pt-undo-position pt))
    (vector-push-extend (make-edit :old old-span :new new-span
                                   :cursors changed-cursors)
                        stack)
    (incf (pt-undo-position pt))
    t))

(defun delete-within-piece (pt cursor count)
  (let* ((tracked-cursors (pt-tracked-cursors pt))
         (stack (pt-undo-stack pt))
         (changed-cursors (make-array 0 :fill-pointer t :adjustable t))
         (piece (cursor-piece cursor))
         (prev (piece-prev piece))
         (next (piece-next piece))
         (old-byte-offset (cursor-byte-offset cursor))
         old-span new-span)
    (when (<= (+ (cursor-byte-offset cursor) count) (piece-size piece))
      (flet ((add-cursor-change (old new)
               (vector-push-extend (make-cursor-edit
                                    :old old
                                    :new (copy-cursor/unlocked new)
                                    :real new)
                                   changed-cursors)))
        (declare (dynamic-extent #'add-cursor-change))
        (macrolet ((with-cursor-updating ((cursor) &body body)
                     `(let ((old (copy-cursor/unlocked ,cursor)))
                        ,@body
                        (add-cursor-change old ,cursor))))
          (let ((deleted-lfs (count-lfs (piece-data piece)
                                        (cursor-byte-offset cursor)
                                        (+ (cursor-byte-offset cursor) count))))
            (map () #'(lambda (tcursor)
                        (cond ((< (cursor-index cursor)
                                  (cursor-index tcursor)
                                  (+ (cursor-index cursor) count))
                               (with-cursor-updating (tcursor)
                                 (setf (cursor-index tcursor) (cursor-index cursor))
                                 (when (cursor-lineno tcursor)
                                   (decf (cursor-lineno tcursor)
                                         (count-lfs (piece-data piece)
                                                    (cursor-byte-offset cursor)
                                                    (cursor-byte-offset tcursor))))))
                              ((>= (cursor-index tcursor) (+ (cursor-index cursor) count))
                               (with-cursor-updating (tcursor)
                                 (decf (cursor-index tcursor) count)
                                 (when (cursor-lineno tcursor)
                                   (decf (cursor-lineno tcursor) deleted-lfs))))))
                 tracked-cursors))
          ;; case 1: whole piece deleted
          (cond ((and (zerop old-byte-offset) (= count (piece-size piece)))
                 ;;(vico-core.evloop:log-event :delete-case-whole-piece)
                 (when (eq (pt-end-cache pt) piece)
                   (setf (pt-end-cache pt) nil))
                 (if (piece-next next)
                     (progn
                       (map () #'(lambda (tcursor)
                                   (when (eq (cursor-piece tcursor) piece)
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) next
                                             (cursor-byte-offset tcursor) 0))))
                            tracked-cursors)
                       (or (cursor-tracked-p cursor)
                           (with-cursor-updating (cursor)
                             (setf (cursor-piece cursor) next
                                   (cursor-byte-offset cursor) 0))))
                     (progn
                       (map () #'(lambda (tcursor)
                                   (when (eq (cursor-piece tcursor) piece)
                                     (with-cursor-updating (tcursor)
                                       (setf (cursor-piece tcursor) prev
                                             (cursor-byte-offset tcursor)
                                             (piece-size prev)))))
                            tracked-cursors)
                       (or (cursor-tracked-p cursor)
                           (with-cursor-updating (cursor)
                             (setf (cursor-piece cursor) prev
                                   (cursor-byte-offset cursor) (piece-size prev))))))
                 (setf old-span (make-span :start piece :end piece
                                           :size (piece-size piece))
                       new-span (make-span)))
                ;; case 2: start on boundary
                ((zerop old-byte-offset)
                 ;;(vico-core.evloop:log-event :delete-case-start-boundary)
                 (let ((new (make-piece :prev prev :next next
                                        :data (inc-ptr (piece-data piece) count)
                                        :size (- (piece-size piece) count))))
                   (map () #'(lambda (tcursor)
                               (when (eq (cursor-piece tcursor) piece)
                                 (with-cursor-updating (tcursor)
                                   (setf (cursor-piece tcursor) new
                                         (cursor-byte-offset tcursor) 0))))
                        tracked-cursors)
                   (or (cursor-tracked-p cursor)
                       (with-cursor-updating (cursor)
                         (setf (cursor-piece cursor) new
                               (cursor-byte-offset cursor) 0)))
                   (when (eq (pt-end-cache pt) piece)
                     (setf (pt-end-cache pt) new))
                   (setf old-span (make-span :start piece :end piece
                                             :size (piece-size piece))
                         new-span (make-span :start new :end new
                                             :size (piece-size new)))))
                ;; case 3: end on boundary
                ((= (+ old-byte-offset count) (piece-size piece))
                 ;;(vico-core.evloop:log-event :delete-case-end-boundary)
                 (when (eq (pt-end-cache pt) piece)
                   (setf (pt-end-cache pt) nil))
                 (let ((new (make-piece :prev prev :next next
                                        :data (piece-data piece)
                                        :size (- (piece-size piece) count))))
                   (if (piece-next next)
                       (progn
                         (map () #'(lambda (tcursor)
                                     (when (eq (cursor-piece tcursor) piece)
                                       (with-cursor-updating (tcursor)
                                         (if (>= (cursor-byte-offset tcursor)
                                                 old-byte-offset)
                                             (setf (cursor-piece tcursor) next
                                                   (cursor-byte-offset tcursor) 0)
                                             ;; 0 <= offset < old-byte-offset
                                             (setf (cursor-piece tcursor) new)))))
                              tracked-cursors)
                         (or (cursor-tracked-p cursor)
                             (with-cursor-updating (cursor)
                               (setf (cursor-piece cursor) next
                                     (cursor-byte-offset cursor) 0))))
                       (progn
                         (map () #'(lambda (tcursor)
                                     (when (eq (cursor-piece tcursor) piece)
                                       (with-cursor-updating (tcursor)
                                         (setf (cursor-piece tcursor) new)
                                         (when (>= (cursor-byte-offset tcursor)
                                                   old-byte-offset)
                                           (setf (cursor-byte-offset tcursor)
                                                 (piece-size new))))))
                              tracked-cursors)
                         (or (cursor-tracked-p cursor)
                             (with-cursor-updating (cursor)
                               (setf (cursor-piece cursor) new
                                     (cursor-byte-offset cursor) (piece-size new))))))
                   (setf old-span (make-span :start piece :end piece
                                             :size (piece-size piece))
                         new-span (make-span :start new :end new
                                             :size (piece-size new)))))
                (t ; case 4: deletion in middle of piece - split in two
                 ;;(vico-core.evloop:log-event :delete-case-middle-of-piece)
                 (let* ((right-boundary (+ old-byte-offset count))
                        (new-left (make-piece :prev prev
                                              :data (piece-data piece)
                                              :size old-byte-offset))
                        (new-right (make-piece :next next
                                               :data (inc-ptr (piece-data piece)
                                                              right-boundary)
                                               :size (- (piece-size piece)
                                                        right-boundary))))
                   (map ()
                        #'(lambda (tcursor)
                            (when (eq (cursor-piece tcursor) piece)
                              (with-cursor-updating (tcursor)
                                (cond ((> (cursor-byte-offset tcursor) ;after end
                                          right-boundary)
                                       (setf (cursor-piece tcursor) new-right)
                                       (decf (cursor-byte-offset tcursor)
                                             right-boundary))
                                      ((<= old-byte-offset
                                           (cursor-byte-offset tcursor)
                                           right-boundary)
                                       (setf (cursor-piece tcursor) new-right
                                             (cursor-byte-offset tcursor) 0))
                                      (t ; < old-byte-offset
                                       (setf (cursor-piece tcursor) new-left))))))
                        tracked-cursors)
                   (or (cursor-tracked-p cursor)
                       (with-cursor-updating (cursor)
                         (setf (cursor-piece cursor) new-right
                               (cursor-byte-offset cursor) 0)))
                   (when (eq (pt-end-cache pt) piece)
                     (setf (pt-end-cache pt) new-right))
                   (setf (piece-next new-left) new-right
                         (piece-prev new-right) new-left)
                   (setf old-span (make-span :start piece :end piece
                                             :size (piece-size piece))
                         new-span (make-span :start new-left :end new-right
                                             :size (+ (piece-size new-left)
                                                      (piece-size new-right)))))))))
      (span-swap pt old-span new-span)
      (setf (fill-pointer stack) (pt-undo-position pt))
      (vector-push-extend (make-edit :old old-span :new new-span
                                     :cursors changed-cursors)
                          stack)
      (incf (pt-undo-position pt))))) ; WHEN ends

(defun delete-at (cursor count)
  (let ((pt (cursor-piece-table cursor)))
    (or (delete-within-piece pt cursor count)
        (delete-multiple pt cursor count)
        (signal-bad-cursor-index cursor (+ (index-at cursor) count)))))

(defmethod buf:delete-at ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (tracked-cursors (pt-tracked-cursors pt)))
    (atomics:atomic-incf (pt-revision pt))
    #+sbcl (sb-thread:barrier (:write))
    (with-pt-lock (pt)
      (unwind-protect ; failsafe in case of index error
           (progn
             (map () #'(lambda (tcursor)
                         (lock-spinlock (cursor-lock tcursor)))
                  tracked-cursors)
             (or (cursor-tracked-p cursor) (lock-spinlock (cursor-lock cursor)))
             (delete-at cursor count))
        ;; cleanup forms
        (map () #'(lambda (tcursor)
                    (incf (cursor-revision tcursor) 2)
                    (unlock-spinlock (cursor-lock tcursor)))
             tracked-cursors)
        (or (cursor-tracked-p cursor)
            (and (incf (cursor-revision cursor) 2)
                 (unlock-spinlock (cursor-lock cursor))))
        #+sbcl (sb-thread:barrier (:write))
        (atomics:atomic-incf (pt-revision pt))))))

(defmethod buf:begin-undo-group ((pt piece-table-buffer))
  (let* ((pt (slot-value pt 'piece-table))
         (stack (pt-undo-stack pt)))
    (unless (pt-undo-grouped-p pt)
      (setf (pt-undo-grouped-p pt) t)
      (vector-push-extend (make-group-edit) stack)
      (incf (pt-undo-position pt)))))

(defmethod buf:end-undo-group ((pt piece-table-buffer))
  (let* ((pt (slot-value pt 'piece-table))
         (stack (pt-undo-stack pt)))
    (when (pt-undo-grouped-p pt)
      (setf (pt-undo-grouped-p pt) nil)
      (vector-push-extend (make-group-edit) stack)
      (incf (pt-undo-position pt)))))

(defmethod buf:undo ((pt piece-table-buffer))
  (let ((pt (slot-value pt 'piece-table)))
    (with-pt-lock (pt)
      (setf (pt-end-cache pt) nil)
      (let ((stack (pt-undo-stack pt)))
        (when (plusp (pt-undo-position pt))
          (atomics:atomic-incf (pt-revision pt))
          #+sbcl (sb-thread:barrier (:write))
          (let ((edit (aref stack (decf (pt-undo-position pt))))
                (tracked-cursors (pt-tracked-cursors pt)))
            (map () #'(lambda (tcursor)
                        (lock-spinlock (cursor-lock tcursor)))
                 tracked-cursors)
            (flet ((undo (edit)
                     (span-swap pt (edit-new edit) (edit-old edit))
                     (loop :with cursors = (edit-cursors edit)
                           :for idx :downfrom (1- (length cursors)) :to 0
                           :for cursor-edit = (aref cursors idx)
                           :for real = (cursor-edit-real cursor-edit)
                           :do (%blit-cursor real (cursor-edit-old cursor-edit)))))
              (etypecase edit
                (edit (undo edit))
                (group-edit
                 (loop :for edit = (aref stack (decf (pt-undo-position pt)))
                       :until (group-edit-p edit)
                       :do (undo edit)))))
            (map () #'(lambda (tcursor)
                        (incf (cursor-revision tcursor) 2)
                        (unlock-spinlock (cursor-lock tcursor)))
                 tracked-cursors))
          #+sbcl (sb-thread:barrier (:write))
          (atomics:atomic-incf (pt-revision pt)))))))

(defmethod buf:redo ((pt piece-table-buffer))
  (let ((pt (slot-value pt 'piece-table)))
    (with-pt-lock (pt)
      (setf (pt-end-cache pt) nil)
      (let ((stack (pt-undo-stack pt)))
        (when (< (pt-undo-position pt) (fill-pointer stack))
          (atomics:atomic-incf (pt-revision pt))
          #+sbcl (sb-thread:barrier (:write))
          (let ((edit (aref stack (pt-undo-position pt)))
                (tracked-cursors (pt-tracked-cursors pt)))
            (map () #'(lambda (tcursor)
                        (lock-spinlock (cursor-lock tcursor)))
                 tracked-cursors)
            (flet ((redo (edit)
                     (span-swap pt (edit-old edit) (edit-new edit))
                     (loop :for cursor-edit :across (edit-cursors edit)
                           :for real = (cursor-edit-real cursor-edit)
                           :do (%blit-cursor real (cursor-edit-new cursor-edit)))))
              (etypecase edit
                (edit (redo edit))
                (group-edit
                 (loop :for edit = (aref stack (incf (pt-undo-position pt)))
                       :until (group-edit-p edit)
                       :do (redo edit))))
              (incf (pt-undo-position pt)))
            (map () #'(lambda (tcursor)
                        (incf (cursor-revision tcursor) 2)
                        (unlock-spinlock (cursor-lock tcursor)))
                 tracked-cursors))
          #+sbcl (sb-thread:barrier (:write))
          (atomics:atomic-incf (pt-revision pt)))))))

;; debugging

(defun pt-string (pt)
  "Checks PT pieces for sanity, returns the whole buffer's contents."
  (assert (loop :for piece = (pt-sentinel-start pt) :then (piece-next piece)
                :until (eq piece (pt-sentinel-end pt))
                :always (eq piece (piece-prev (piece-next piece)))))
  (let ((enc::*suppress-character-coding-errors* t)
        (string ""))
    (loop :for piece = (pt-sentinel-start pt) :then (piece-next piece)
          :until (eq piece (pt-sentinel-end pt))
          :do (setf string
                    (concatenate 'string string
                                 (ffi:foreign-string-to-lisp (piece-data piece)
                                                             :count (piece-size piece)))))
    string))

(defun piece-list (pt)
  (loop :for piece = (pt-sentinel-start pt) :then (piece-next piece)
        :while piece
        :collect piece))

(defun count-pieces (pt)
  (- (length (piece-list pt)) 2))


;; (assert (string= (pt-string *pt*)
;;                  (format nil "test~%~%bαf~%")))

;; (defparameter cursor (buf:make-cursor *pt* 0))

;; (assert (string= (with-output-to-string (s)
;;                    (loop for i below (pt-size *pt*)
;;                          do (handler-case
;;                                 (let ((c (buf:make-cursor *pt* i)))
;;                                   (princ (char-at c) s)
;;                                   (princ (line-at c) s))
;;                               (enc:character-decoding-error ()))))
;;                  (format nil "t1e1s1t1~%1b2α2f2~%2")))

;; (with-open-file (s "~/common-lisp/misc-vico/empty" :element-type '(unsigned-byte 8))
;;   (defparameter test (buf:make-buffer :piece-table :initial-stream s)))
;; (defparameter cs (buf:make-cursor test 0))
;; (buf:insert-at cs "t")
;; (buf:insert-at cs "t")

;; (time
;;  (with-open-file (s "~/common-lisp/misc-vico/test.xml" :element-type '(unsigned-byte 8))
;;    (defparameter test (buf:make-buffer :piece-table :initial-stream s))))

;; (let* ((ppcre:*use-bmh-matchers* t)
;;        (scanner (ppcre:create-scanner "thing"))
;;        (cursor (buf:make-cursor test 0)))
;;   (tg:gc :full t)
;;   (time
;;    (loop
;;      (let ((len (buf:cursor-search-next cursor scanner)))
;;        (or len (return))
;;        (buf:delete-at cursor len)
;;        (buf:insert-at cursor "thang")))))

;; (let* ((ppcre:*use-bmh-matchers* t)
;;        (scanner (ppcre:create-scanner (reverse-regex "thing")))
;;        (cursor (buf:make-cursor test (pt-size test))))
;;   (tg:gc :full t)
;;   (time
;;    (loop
;;      (let ((len (buf:cursor-search-prev cursor scanner)))
;;        (or len (return))
;;        (buf:delete-at cursor len)
;;        (buf:insert-at cursor "thang")))))
