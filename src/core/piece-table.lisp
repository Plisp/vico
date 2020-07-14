;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; piece table rewrite
;;
;; DONE implement interface
;; DONE mmap()
;; DONE error handling
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

;; credit: Shinmera
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
                         (data-buffer-type db)
                         contents
                         (= (length contents) 100))))))
  "manages a pointer to a foreign vector of octets"
  (size 0 :type idx)
  (capacity 0 :type idx :read-only t)
  (data (ffi:null-pointer) :type ffi:foreign-pointer :read-only t)
  (type :alloc :read-only t))

(defun data-buffer-free (data-buffer)
  (let ((type (data-buffer-type data-buffer)))
    (cond ((eq type :alloc)
           (ffi:foreign-string-free (data-buffer-data data-buffer)))
          ((and (listp type) (eq (car type) :mmap))
           (apply #'mmap:munmap (cdr type))))))

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

(defstruct (piece-table (:conc-name pt-))
  (size 0 :type idx)
  (data-buffers (list) :type list)
  (end-cache nil)
  (sentinel-start (make-piece) :read-only t)
  (sentinel-end (make-piece) :read-only t)
  (closed-p nil)
  (line-cache 0 :type idx)
  (line-cache-valid-p nil)
  ;; TODO maybe sorted - order kept during insertions, so we can update with local info
  (tracked-cursors (make-array 3 :fill-pointer 0 :adjustable t) :type vector)
  ;; must be held around accesses to the buffer's pieces/data-buffers/tracked-cursors to
  ;; safeguard the the integrity of the tracked-cursors list and also the correctness
  ;; of cursors - i.e. MAKE-CURSOR
  (lock nil :type t)
  (revision 0 :type #+ecl fixnum #+sbcl sb-ext:word #+ccl t))

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

(defmethod buf:make-buffer ((type (eql :piece-table)) &key (initial-contents "")
                                                        initial-stream)
  (let (init-buffer init-char* init-length)
    (if (io:file-stream-p initial-stream)
        (let (fd)
          (setf init-length (file-length initial-stream))
          (multiple-value-setq (init-char* fd)
            (mmap:mmap (probe-file initial-stream) :size init-length))
          (setf init-buffer
                (make-data-buffer :size init-length
                                  :capacity init-length
                                  :data init-char*
                                  :type (list :mmap init-char* fd init-length))))
        (progn
          (multiple-value-setq (init-char* init-length)
            (ffi:foreign-string-alloc initial-contents :null-terminated-p t))
          (decf init-length) ;TODO null terminator variable length, encodings
          (setf init-buffer (make-data-buffer :size init-length
                                              :capacity init-length
                                              :data init-char*))))
    (let* ((init-piece (make-piece :size init-length :data init-char*))
           (pt (make-piece-table :size init-length
                                 :data-buffers (list init-buffer))))
      (setf (piece-next (pt-sentinel-start pt)) init-piece
            (piece-prev init-piece) (pt-sentinel-start pt)
            (piece-prev (pt-sentinel-end pt)) init-piece
            (piece-next init-piece) (pt-sentinel-end pt))
      pt)))

(defmethod buf:copy-buffer ((pt piece-table))
  ;;(copy-piece-table pt) ;TODO deep copy required, be careful, is this even needed now?
  (error "not yet implemented"))

(defmethod buf:close-buffer ((pt piece-table))
  (unless (pt-closed-p pt)
    (setf (pt-closed-p pt) t)
    ;; Here we invalidate all cursors, meaning no more future accesses to the buffer,
    ;; but it will be safe until other threads finish. Once cursors finish, references
    ;; to them should be dropped and the finalizer will run (eventually).
    ;; TODO document that cursor-invalid may be due to buffer being closed, meaning
    ;; cursors referencing the buffer should be un-referenced
    (atomics:atomic-incf (pt-revision pt)) ; revision becomes odd - permanently invalid
    (let ((data-buffers (pt-data-buffers pt)))
      (tg:finalize pt (lambda () (map () #'data-buffer-free data-buffers))))))

(defmethod buf:size ((pt piece-table))
  (pt-size pt))

(declaim (ftype (function (t idx idx) idx) count-lfs))
(defun count-lfs (piece start end)
  "note: accesses foreign memory, must be locked"
  (declare (optimize speed)
           (type idx start end))
  (loop :with lfs :of-type idx
        :with len :of-type idx = (- end start)
        :with ptr = (inc-ptr (piece-data piece) start)
        :do (let ((found (ffi:foreign-funcall "memchr"
                                              :pointer ptr
                                              :int #.(char-code #\newline)
                                              size-t len
                                              :pointer)))
              (when (ffi:null-pointer-p found)
                (return lfs))
              (incf lfs)
              (decf len (1+ (- (the idx (ffi:pointer-address found))
                               (the idx (ffi:pointer-address ptr)))))
              (setf ptr (inc-ptr found 1)))))

(defun pt-line-count (pt)
  (if (pt-line-cache-valid-p pt)
      (pt-line-cache pt)
      (setf (pt-line-cache-valid-p pt) t
            (pt-line-cache pt) (loop :with line :of-type idx
                                     :for piece = (pt-sentinel-start pt)
                                       :then (piece-next piece)
                                     :while (piece-next piece)
                                     :do (incf line (count-lfs piece 0 (piece-size piece)))
                                     :finally (return line)))))

(defmethod buf:line-count ((pt piece-table))
  (pt-line-count pt))

(defmethod buf:edit-timestamp ((pt piece-table))
  (pt-revision pt))

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

;; we could cache line numbers for people who like line numbering...
;; but it's pretty fast already and that would slow down cursor iteration
;; anyways it's pretty trivial so TODO
(defstruct (cursor (:copier %copy-cursor)
                   (:print-object
                    (lambda (cursor stream)
                      (format stream "#S(CURSOR ~@[static ~]~@[un~]tracked :piece ~a ~
                                         :byte-offset ~d :index ~d :revision ~d)"
                              (cursor-static-p cursor)
                              (not (cursor-tracked-p cursor))
                              (cursor-piece cursor)
                              (cursor-byte-offset cursor)
                              (cursor-index cursor)
                              (cursor-revision cursor)))))
  (piece-table nil :read-only t)
  piece
  (byte-offset 0 :type idx)
  (index 0 :type idx)
  (lock nil :type t) ; used to safeguard COPY-CURSOR
  (revision 0 :type idx)
  (tracked-p nil :type boolean) ;if T, we can always access
  static-p)

(defmethod buf:make-cursor ((pt piece-table) index &key track static)
  (with-pt-lock (pt)
    (when (> index (pt-size pt))
      (error 'conditions:vico-bad-index
             :buffer pt
             :bad-index index
             :bounds (cons 0 (pt-size pt))))
    (loop :with revision = (pt-revision pt)
          :with piece-index :of-type idx
          :for piece = (piece-next (pt-sentinel-start pt)) :then (piece-next piece)
          :while (piece-next piece)
          :when (<= piece-index index (+ piece-index (1- (piece-size piece))))
            :return (let ((cursor (make-cursor :piece-table pt
                                               :piece piece
                                               :byte-offset (- index piece-index)
                                               :index index
                                               :revision revision
                                               :tracked-p track
                                               :static-p static)))
                      (when track
                        (vector-push-extend cursor (pt-tracked-cursors
                                                    (cursor-piece-table cursor))))
                      cursor)
          :do (incf piece-index (piece-size piece))
          :finally (when (= piece-index index) ;off-end
                     (let ((cursor (make-cursor :piece-table pt
                                                :piece (piece-prev piece)
                                                :byte-offset (piece-size (piece-prev piece))
                                                :index index
                                                :revision revision
                                                :tracked-p track
                                                :static-p static)))
                       (when track
                         (vector-push-extend cursor (pt-tracked-cursors
                                                     (cursor-piece-table cursor))))
                       (return cursor))))))

(defun copy-cursor (cursor)
  (let ((copy (with-cursor-lock (cursor) (%copy-cursor cursor))))
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
  (def-cursor-arith >=))

(defun %blit-cursor (dest src)
  "CAREFUL: very dangerous. Only use if at least one cursor is private to your thread.
Any cursor which is not private must be locked. They must correspond to the same buffer."
  (setf (cursor-byte-offset dest) (cursor-byte-offset src)
        (cursor-piece dest) (cursor-piece src)
        (cursor-index dest) (cursor-index src)))

(defmethod buf:cursor-buffer ((cursor cursor)) ;unlocked
  (cursor-piece-table cursor))

(defun get-revision (pt)
  (let ((pre-revision (pt-revision pt)))
    #+sbcl (sb-thread:barrier (:read))
    (logand pre-revision (lognot 1)))) ;odd counts reduced by 1 to auto-invalidate

(defun check-revision (pt revision)
  #+sbcl (sb-thread:barrier (:read))
  (= (pt-revision pt) revision))

(defmethod buf:cursor-static-p ((cursor cursor))
  (cursor-static-p cursor))

(defmethod (setf buf:cursor-static-p) (new-value (cursor cursor))
  (setf (cursor-static-p cursor) new-value))

(defmethod buf:cursor-tracked-p ((cursor cursor))
  (cursor-tracked-p cursor))

(defmethod (setf buf:cursor-tracked-p) (new-value (cursor cursor))
  (with-pt-lock ((cursor-piece-table cursor))
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt)))
      (setf (cursor-tracked-p cursor) new-value)
      (let ((tracked-cursors (pt-tracked-cursors (cursor-piece-table cursor))))
        (if new-value
            (vector-push-extend cursor tracked-cursors)
            (let ((position (position cursor tracked-cursors)))
              (shiftf (aref tracked-cursors position)
                      (aref tracked-cursors (decf (fill-pointer tracked-cursors)))))))
      (or (check-revision pt pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor)))))

;; readers

(declaim (inline index-at))
(defun index-at (cursor)
  (cursor-index cursor))

(defmethod buf:index-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (index (index-at cursor)))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    index))

(declaim (ftype (function (t) idx) line-at))
(defun line-at (cursor)
  (declare (optimize speed (safety 0)))
  (loop :with line :of-type idx
          = (count-lfs (cursor-piece cursor) 0 (cursor-byte-offset cursor))
        :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
        :while (piece-prev piece)
        :do (incf line (count-lfs piece 0 (piece-size piece)))
        :finally (return (1+ line))))

(defmethod buf:line-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (line (line-at cursor)))
    (or (check-revision pt pre-revision)
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
    (or (check-revision pt pre-revision)
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

;; TODO should probably error on invalid indexes
(defmethod buf:char-at ((cursor cursor))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (char (char-at cursor)))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    char))

;; movement

;; note to self: the IF branches are not necessary, but are possibly clearer and more
;; efficient (likely branch, less+) - reflects byte-offset = 0 in subsequent iterations

(defun cursor-next (cursor count)
  (declare (optimize speed)
           (type idx count))
  #-sbcl (check-type count idx)
  (if (< (+ (cursor-byte-offset cursor) count) (piece-size (cursor-piece cursor)))
      (incf (cursor-byte-offset cursor) count)
      (loop :initially (decf left (the idx (- (piece-size (cursor-piece cursor))
                                              (cursor-byte-offset cursor))))
            :with left :of-type idx = count
            :for piece = (piece-next (cursor-piece cursor)) :then (piece-next piece)
            :while (and (>= left (piece-size piece)) (piece-next piece))
            :do (decf left (piece-size piece))
            :finally (cond ((piece-next piece)
                            (setf (cursor-piece cursor) piece
                                  (cursor-byte-offset cursor) left))
                           ((zerop left) ;end-of-buffer
                            (setf (cursor-piece cursor) (piece-prev piece)
                                  (cursor-byte-offset cursor) (piece-size
                                                               (cursor-piece cursor))))
                           (t (return-from cursor-next t)))))
  (incf (cursor-index cursor) count)
  nil)

(defmethod buf:cursor-next ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-next cursor count))))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (buf:signal-bad-cursor-index cursor (+ (index-at cursor) count))))
  cursor)

;; same as above, but byte-offset = piece-size in subsequent iterations

(defun cursor-prev (cursor count)
  (declare (optimize speed)
           (type idx count))
  #-sbcl (check-type count idx)
  (if (>= (cursor-byte-offset cursor) count)
      (decf (cursor-byte-offset cursor) count)
      (loop :initially (decf left (cursor-byte-offset cursor))
            :with left :of-type idx = count
            :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
            :while (and (> left (piece-size piece)) (piece-prev piece))
            :do (decf left (piece-size piece))
            :finally (if (piece-prev piece)
                         (setf (cursor-piece cursor) piece
                               (cursor-byte-offset cursor) (- (piece-size piece) left))
                         (return-from cursor-prev t))))
  (decf (cursor-index cursor) count)
  nil)

(defmethod buf:cursor-prev ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-prev cursor count))))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (buf:signal-bad-cursor-index cursor (- (index-at cursor) count))))
  cursor)

;;  0     1         2      3 4
;; .\nx...\n....|...\n.....\nx.|...
;; 3  ^                         ^
;; TODO line caching O(n) (n=size of text) is not good enough

(defun cursor-next-line (cursor count)
  (declare (optimize speed)
           (type idx count))
  #-sbcl (check-type count idx)
  (loop :with remaining-count = count
        :with copy = (%copy-cursor cursor)
        :for piece = (cursor-piece copy) :then (piece-next piece)
        :for start = (inc-ptr (piece-data piece) (cursor-byte-offset copy))
        :do (loop :with found :of-type idx
                  :for piece-end :of-type idx
                    = (+ (the idx (ffi:pointer-address (piece-data piece)))
                         (piece-size piece))
                  :for left :of-type idx = (- piece-end
                                              (the idx (ffi:pointer-address start)))
                  :while (and (plusp remaining-count) (plusp left))
                  :do (setf found (ffi:pointer-address
                                   (ffi:foreign-funcall "memchr"
                                                        :pointer start
                                                        :int #.(char-code #\newline)
                                                        size-t left
                                                        :pointer)))
                      (when (zerop found) ; NULL
                        (cursor-next copy left)
                        (return))
                      (let ((inc (1+ (- found (the idx (ffi:pointer-address start))))))
                        (cursor-next copy inc)
                        (setf start (inc-ptr start inc)))
                      (decf remaining-count))
        :while (and (plusp remaining-count) (piece-next piece)) ;not sentinel
        :finally (or (piece-next piece)
                     (return-from cursor-next-line t))
                 (%blit-cursor cursor copy)))

(defmethod buf:cursor-next-line ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-next-line cursor count))))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (buf:signal-bad-cursor-line cursor (+ (line-at cursor) count))))
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
                  :for start-ptr = (piece-data piece)
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
                              (cursor-index copy) 0)
                        (%blit-cursor cursor copy))
                       (t (return-from cursor-prev-line t)))))

(defmethod buf:cursor-prev-line ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-prev-line cursor count))))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (buf:signal-bad-cursor-line cursor (- (line-at cursor) count))))
  cursor)

;; TODO support arbitrary encodings, memoize mappings in struct. assuming UTF-8 for now
;; PR for babel to have leading-byte test

(defun cursor-next-char (cursor count)
  (declare (optimize speed)
           (type idx count))
  (loop :repeat count
        :with copy = (%copy-cursor cursor)
        :do (loop
              (when (cursor-next copy 1)
                (return-from cursor-next-char (1+ (cursor-index copy))))
              (let ((byte (byte-at copy)))
                (when (/= (logand byte #xC0) #x80) ; leading utf-8 byte
                  (return))))
        :finally (%blit-cursor cursor copy)))

(defmethod buf:cursor-next-char ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-next-char cursor count))))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (buf:signal-bad-cursor-index cursor result)))
  cursor)

(defun cursor-prev-char (cursor count)
  (declare (optimize speed)
           (type idx count))
  (loop :repeat count
        :with copy = (%copy-cursor cursor)
        :do (loop
              (when (cursor-prev copy 1)
                (return-from cursor-prev-char (1- (cursor-index copy))))
              (let ((byte (byte-at copy)))
                (when (/= (logand byte #xC0) #x80)
                  (return))))
        :finally (%blit-cursor cursor copy)))

(defmethod buf:cursor-prev-char ((cursor cursor) &optional (count 1))
  (let* ((pt (cursor-piece-table cursor))
         (pre-revision (get-revision pt))
         (result (with-cursor-lock (cursor)
                   (cursor-prev-char cursor count))))
    (or (check-revision pt pre-revision)
        (error 'conditions:vico-cursor-invalid :cursor cursor))
    (when result
      (buf:signal-bad-cursor-index cursor result)))
  cursor)

(defmethod buf:cursor-find-next ((cursor cursor) char)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (result (loop :until (char= (char-at copy) char)
                         :do (when (cursor-next-char copy 1)
                               (return t))
                         :finally (%blit-cursor cursor copy))))
      (or (check-revision pt pre-revision)
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
                         :until (char= (char-at copy) char)
                         :finally (%blit-cursor cursor copy))))
      (or (check-revision pt pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      (if result
          nil
          (values char cursor)))))

;; TODO use custom search routines here (or use ppcre:quote-meta-chars), separate regex
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
                    (cons :sequence (nreverse (recur (cdr o)))))
                   (t
                    (map 'list #'recur o)))))
    (recur (ppcre:parse-string regex))))

(defmethod buf:cursor-search-next ((cursor cursor) string)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (char-offset 0)
           (result
             (block nil
               (let* ((ppcre:*use-bmh-matchers* t)
                      (fn (lambda (buffer index)
                            (declare (ignore buffer))
                            (let* ((delta (- index char-offset)))
                              (when (if (plusp delta)
                                        (cursor-next-char copy delta)
                                        (cursor-prev-char copy (- delta)))
                                (return t))
                              (incf char-offset delta))
                            (char-at copy))))
                 (if-let (index (ppcre:scan string pt :accessor fn :end (pt-size pt)))
                   (cursor-next-char cursor index)
                   t))))) ; not found
      (or (check-revision pt pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      (if result nil cursor))))

(defmethod buf:cursor-search-prev ((cursor cursor) string)
  (with-cursor-lock (cursor)
    (let* ((pt (cursor-piece-table cursor))
           (pre-revision (get-revision pt))
           (copy (%copy-cursor cursor))
           (char-backwards-offset 0)
           (result
             (block nil
               (let ((ppcre:*use-bmh-matchers* t)
                     (fn (lambda (buffer index) ; provide a reversed stream
                           (declare (ignore buffer))
                           (let ((delta (- index char-backwards-offset)))
                             (when (if (plusp delta)
                                       (cursor-prev-char copy delta)
                                       (cursor-next-char copy (- delta)))
                               (return t))
                             (incf char-backwards-offset delta))
                           (char-at copy))))
                 (if-let (index (nth-value 1 (ppcre:scan (reverse-regex string) pt
                                                         :accessor fn
                                                         :start 0
                                                         :end (cursor-index cursor))))
                   (cursor-prev-char cursor (1- index))
                   t)))))
      (or (check-revision pt pre-revision)
          (error 'conditions:vico-cursor-invalid :cursor cursor))
      (if result nil cursor))))

;; modification

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
               (new-buffer (make-data-buffer :size strlen
                                             :capacity new-capacity
                                             :data new-char*
                                             :type :alloc)))
          (ffi:lisp-string-to-foreign string new-char*
                                      (1+ strlen)) ;no. bytes including null

          (push new-buffer (pt-data-buffers pt))
          (values new-char* strlen nil))
        (let ((new-char* (ffi:lisp-string-to-foreign string (data-buffer-data buf)
                                                     (1+ strlen) ;no. bytes including null
                                                     :offset (data-buffer-size buf))))
          (incf (data-buffer-size buf) strlen)
          (values new-char* strlen t)))))

(defmethod buf:insert-at ((cursor cursor) string)
  (unless (zerop (length string))
    (let* ((pt (cursor-piece-table cursor))
           (tracked-cursors (pt-tracked-cursors pt))
           (static (cursor-static-p cursor))
           (piece (cursor-piece cursor))
           (prev (piece-prev piece)))
      (setf (pt-line-cache-valid-p pt) nil) ; race, race, race!
      (atomics:atomic-incf (pt-revision pt))
      (with-pt-lock (pt)
        (multiple-value-bind (new-ptr strlen appendedp)
            (data-buffer-append pt string)
          (map () #'(lambda (tcursor)
                      (lock-spinlock (cursor-lock tcursor)))
               tracked-cursors)
          (or (cursor-tracked-p cursor)
              (lock-spinlock (cursor-lock cursor)))
          (incf (pt-size pt) strlen)
          (cond ((zerop (cursor-byte-offset cursor)) ;boundary case
                 (if (and (eq prev (pt-end-cache pt)) appendedp)
                     (progn ; TODO parallelize if needed
                       (map () #'(lambda (tcursor)
                                   (when (and (cursor= tcursor cursor)
                                              (cursor-static-p tcursor))
                                     (setf (cursor-piece tcursor) prev
                                           (cursor-byte-offset tcursor)
                                           (piece-size prev))))
                            tracked-cursors)
                       (when static
                         (setf (cursor-piece cursor) prev
                               (cursor-byte-offset cursor) (piece-size prev)))
                       (incf (piece-size prev) strlen))
                     ;; new
                     (let ((new-piece (make-piece :prev prev :next piece
                                                  :data new-ptr
                                                  :size strlen)))
                       (map () #'(lambda (tcursor)
                                   (when (and (cursor= tcursor cursor)
                                              (cursor-static-p tcursor))
                                     ;; we know offset = 0 already
                                     (setf (cursor-piece tcursor) new-piece)))
                            tracked-cursors)
                       (when static
                         (setf (cursor-piece cursor) new-piece))
                       ;;
                       (setf (pt-end-cache pt) new-piece)
                       (setf (piece-next prev) new-piece
                             (piece-prev piece) new-piece))))
                ((< (cursor-byte-offset cursor) (piece-size piece))
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
                   (map () #'(lambda (tcursor)
                               (when (eq (cursor-piece tcursor) piece)
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
                                        (setf (cursor-piece tcursor) left-split)))))
                        tracked-cursors)
                   (setf (cursor-piece cursor) (if static new-piece right-split)
                         (cursor-byte-offset cursor) 0)
                   ;;
                   (setf (pt-end-cache pt) new-piece)
                   (setf (piece-prev new-piece) left-split
                         (piece-next new-piece) right-split
                         (piece-next prev) left-split
                         (piece-prev next) right-split)))
                (t ; off-end
                 (if (and (eq piece (pt-end-cache pt)) appendedp)
                     (progn
                       (incf (piece-size piece) strlen)
                       ;; set non-static off-end cursors to new size
                       (map () #'(lambda (tcursor)
                                   (when (and (= (cursor-index tcursor)
                                                 (cursor-index cursor))
                                              (not (cursor-static-p tcursor)))
                                     (setf (cursor-byte-offset tcursor)
                                           (piece-size piece))))
                            tracked-cursors)
                       (or static
                           (setf (cursor-byte-offset cursor) (piece-size piece))))
                     ;; new
                     (let* ((sentinel (piece-next piece))
                            (new-piece (make-piece :prev piece :next sentinel
                                                   :data new-ptr
                                                   :size strlen)))
                       (map () #'(lambda (tcursor)
                                   (when (= (cursor-index tcursor)
                                            (cursor-index cursor))
                                     (setf (cursor-piece tcursor) new-piece)
                                     (setf (cursor-byte-offset tcursor)
                                           (if (cursor-static-p tcursor)
                                               0
                                               (piece-size new-piece)))))
                            tracked-cursors)
                       (setf (cursor-piece cursor) new-piece)
                       (setf (cursor-byte-offset cursor)
                             (if static 0 (piece-size new-piece)))
                       ;;
                       (setf (pt-end-cache pt) new-piece)
                       (setf (piece-next piece) new-piece
                             (piece-prev sentinel) new-piece)))))
          (map () #'(lambda (tcursor)
                      (when (and (cursor>= tcursor cursor)
                                 (not (cursor-static-p tcursor)))
                        (incf (cursor-index tcursor) strlen)))
               tracked-cursors)
          (or static (cursor-tracked-p cursor)
              (incf (cursor-index cursor) strlen))
          ;; cleanup
          (map () #'(lambda (tcursor)
                      (incf (cursor-revision tcursor) 2)
                      (unlock-spinlock (cursor-lock tcursor)))
               tracked-cursors)
          (or (cursor-tracked-p cursor)
              (progn
                (incf (cursor-revision cursor) 2)
                (unlock-spinlock (cursor-lock cursor))))
          (atomics:atomic-incf (pt-revision pt))))
      pt)))

;; TODO verify correctness, writing tests
(defun delete-multiple (pt cursor count)
  "Deletion spanning multiple pieces"
  (let* ((tracked-cursors (pt-tracked-cursors pt))
         (piece (cursor-piece cursor))
         (prev (piece-prev piece))
         (old-byte-offset (cursor-byte-offset cursor))
         new-piece new-last)
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
      ;; TODO we destroy info about cursor position here, better save it for undo
      ;; 1) update cursors in PIECE (don't forget cursors before the break)
      ;; 2) update cursors in intermediate pieces
      ;; 3) update cursors in LAST (don't forget cursors after the break)
      ;; 4) update CURSOR itself
      (let ((last-piece-p (not (piece-next (piece-next last))))
            (last->next (piece-next last)))
        (if (or new-last (not last-piece-p))
            (progn ; we're not deleting to the end
              ;;(vico-core.evloop:log-event :delete-case-multiple-not-end)
              (or new-last (setf new-last (piece-next last)))
              (map () #'(lambda (tcursor)
                          (when (eq (cursor-piece tcursor) piece)
                            (if (>= (cursor-byte-offset tcursor) old-byte-offset)
                                (setf (cursor-piece tcursor) new-last
                                      (cursor-byte-offset tcursor) 0)
                                (setf (cursor-piece tcursor) new-piece))))
                   tracked-cursors)
              (loop :for p = (piece-next piece) :then (piece-next p)
                    :until (eq p last)
                    :do (map () #'(lambda (tcursor)
                                    (when (eq (cursor-piece tcursor) p)
                                      (setf (cursor-piece tcursor) new-last
                                            (cursor-byte-offset tcursor) 0)))
                             tracked-cursors))
              (map () #'(lambda (tcursor)
                          (when (eq (cursor-piece tcursor) last)
                            (setf (cursor-piece tcursor) new-last)
                            (if (> (cursor-byte-offset tcursor) last-offset)
                                (decf (cursor-byte-offset tcursor) last-offset)
                                (setf (cursor-byte-offset tcursor) 0))))
                   tracked-cursors)
              (setf (cursor-piece cursor) new-last
                    (cursor-byte-offset cursor) 0))
            (flet ((update-cursor (tcursor) ; we deleted the last piece
                     (setf (cursor-piece tcursor) new-piece
                           (cursor-byte-offset tcursor) (piece-size new-piece))))
              (declare (dynamic-extent #'update-cursor))
              ;;(vico-core.evloop:log-event :delete-case-multiple-to-end)
              (or new-piece (setf new-piece prev))
              (map () #'(lambda (tcursor)
                          (when (eq (cursor-piece tcursor) piece)
                            (update-cursor tcursor)))
                   tracked-cursors)
              (loop :for p = (piece-next piece) :then (piece-next p)
                    :until (eq p last)
                    :do (map () #'(lambda (tcursor)
                                    (when (eq (cursor-piece tcursor) p)
                                      (update-cursor tcursor)))
                             tracked-cursors))
              (map () #'(lambda (tcursor)
                          (when (eq (cursor-piece tcursor) last)
                            (setf (cursor-piece tcursor) new-piece
                                  (cursor-byte-offset tcursor) (piece-size new-piece))))
                   tracked-cursors)
              (update-cursor cursor)))
        (if new-last
            (if new-piece
                (setf (piece-next prev) new-piece
                      (piece-next new-piece) new-last
                      (piece-prev last->next) new-last
                      (piece-prev new-last) new-piece)
                (setf (piece-next prev) new-last
                      (piece-prev last->next) new-last
                      (piece-prev new-last) prev))
            (if new-piece
                (setf (piece-next prev) new-piece
                      (piece-next new-piece) last->next
                      (piece-prev last->next) new-piece)
                (setf (piece-next prev) last->next
                      (piece-prev last->next) prev)))))
    t))

(defun delete-within-piece (pt cursor count)
  (let* ((tracked-cursors (pt-tracked-cursors pt))
         (piece (cursor-piece cursor))
         (prev (piece-prev piece))
         (next (piece-next piece))
         (old-byte-offset (cursor-byte-offset cursor)))
    (when (<= (+ (cursor-byte-offset cursor) count) (piece-size piece))
      ;; case 1: whole piece deleted
      (cond ((and (zerop old-byte-offset) (= count (piece-size piece)))
             ;;(vico-core.evloop:log-event :delete-case-whole-piece)
             (when (eq (pt-end-cache pt) piece)
               (setf (pt-end-cache pt) nil))
             (if (piece-next next)
                 (progn
                   (map () #'(lambda (tcursor)
                               (when (eq (cursor-piece tcursor) piece)
                                 (setf (cursor-piece tcursor) next
                                       (cursor-byte-offset tcursor) 0)))
                        tracked-cursors)
                   (setf (cursor-piece cursor) next
                         (cursor-byte-offset cursor) 0))
                 (progn
                   (map () #'(lambda (tcursor)
                               (when (eq (cursor-piece tcursor) piece)
                                 (setf (cursor-piece tcursor) prev
                                       (cursor-byte-offset tcursor) (piece-size prev))))
                        tracked-cursors)
                   (setf (cursor-piece cursor) prev
                         (cursor-byte-offset cursor) (piece-size prev))))
             (setf (piece-prev next) prev
                   (piece-next prev) next))
            ;; case 2: start on boundary
            ((zerop old-byte-offset)
             ;;(vico-core.evloop:log-event :delete-case-start-boundary)
             (let ((new (make-piece :prev prev :next next
                                    :data (inc-ptr (piece-data piece) count)
                                    :size (- (piece-size piece) count))))
               (map () #'(lambda (tcursor)
                           (when (eq (cursor-piece tcursor) piece)
                             (setf (cursor-piece tcursor) new
                                   (cursor-byte-offset tcursor) 0)))
                    tracked-cursors)
               (setf (cursor-piece cursor) new
                     (cursor-byte-offset cursor) 0)
               (when (eq (pt-end-cache pt) piece)
                 (setf (pt-end-cache pt) new))
               ;; relink
               (setf (piece-next prev) new
                     (piece-prev next) new)))
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
                                   (if (>= (cursor-byte-offset tcursor)
                                           old-byte-offset)
                                       (setf (cursor-piece tcursor) next
                                             (cursor-byte-offset tcursor) 0)
                                       ;; 0 <= offset < old-byte-offset
                                       (setf (cursor-piece tcursor) new))))
                          tracked-cursors)
                     (setf (cursor-piece cursor) next
                           (cursor-byte-offset cursor) 0))
                   (progn
                     (map () #'(lambda (tcursor)
                                 (when (eq (cursor-piece tcursor) piece)
                                   (setf (cursor-piece tcursor) new)
                                   (when (>= (cursor-byte-offset tcursor)
                                             old-byte-offset)
                                     (setf (cursor-byte-offset tcursor)
                                           (piece-size new)))))
                          tracked-cursors)
                     (setf (cursor-piece cursor) new
                           (cursor-byte-offset cursor) (piece-size new))))
               (setf (piece-prev next) new
                     (piece-next prev) new)))
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
                                 (setf (cursor-piece tcursor) new-left)))))
                    tracked-cursors)
               (setf (cursor-piece cursor) new-right
                     (cursor-byte-offset cursor) 0)
               (when (eq (pt-end-cache pt) piece)
                 (setf (pt-end-cache pt) new-right))
               ;; relink
               (setf (piece-next new-left) new-right
                     (piece-prev new-right) new-left
                     (piece-next prev) new-left
                     (piece-prev next) new-right))))
      t))) ; WHEN ends

;; TODO for undo/redo, store prev and swapped pieces
(defmethod buf:delete-at ((cursor cursor) &optional count)
  (let* ((pt (cursor-piece-table cursor))
         (tracked-cursors (pt-tracked-cursors pt)))
    (setf (pt-line-cache-valid-p pt) nil) ; race race race!
    (atomics:atomic-incf (pt-revision pt))
    (with-pt-lock (pt)
      (decf (pt-size pt) count)
      (unwind-protect
           (progn
             (or (cursor-tracked-p cursor)
                 (lock-spinlock (cursor-lock cursor)))
             (map () #'(lambda (tcursor)
                         (lock-spinlock (cursor-lock tcursor)))
                  tracked-cursors)
             (or (delete-within-piece pt cursor count)
                 (delete-multiple pt cursor count)
                 (buf:signal-bad-cursor-index cursor (+ (index-at cursor) count)))
             (map () #'(lambda (tcursor)
                         (when (< (cursor-index cursor)
                                  (cursor-index tcursor)
                                  (+ 1 (cursor-index cursor) count))
                           (setf (cursor-index tcursor) (cursor-index cursor))))
                  tracked-cursors))
        ;; cleanup forms
        (map () #'(lambda (tcursor)
                    (incf (cursor-revision tcursor) 2)
                    (unlock-spinlock (cursor-lock tcursor)))
             tracked-cursors)
        (or (cursor-tracked-p cursor)
            (and (incf (cursor-revision cursor) 2)
                 (unlock-spinlock (cursor-lock cursor))))
        (atomics:atomic-incf (pt-revision pt))))))

;; debugging

;; (when (boundp '*pt*)
;;   (buf:close-buffer *pt*))
;; (defparameter *pt* (buf:make-buffer :piece-table
;;                                     :initial-contents (format nil "test~%~%bαf~%")))

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
