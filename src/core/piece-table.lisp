;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; piece table rewrite
;;
;; DONE implement interface
;; TODO mmap()
;; DONE error handling
;; TODO encoding support with babel
;;

(defpackage :vico-core.buffer.piece-table
  (:use :cl :alexandria)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:enc :babel-encodings)
                    (:ffi :cffi)))
(in-package :vico-core.buffer.piece-table)

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
                 (format stream "#S(data-buffer object with size: ~d capacity: ~d ~
                                    type: ~s contains: ~a~@[...~])"
                         (data-buffer-size db)
                         (data-buffer-capacity db)
                         (data-buffer-type db)
                         contents
                         (= (length contents) 100))))))
  "manages a pointer to a foreign vector of octets"
  (size 0 :type idx)
  (capacity 0 :type idx :read-only t)
  (data (ffi:null-pointer) :type ffi:foreign-pointer :read-only t)
  (type :alloc :type (member :mmap :alloc) :read-only t))

(defun data-buffer-insert (pt string)
  "Inserts STRING into PT's first data buffer, or makes a new one if capacity is full.
Returns a pointer and byte length of STRING encoded in PT's encoding.
By far the ugliest piece of code here...*surprise surprise* *lisp weenie noises*"
  (let ((strlen (babel:string-size-in-octets string
                                             :encoding ffi:*default-foreign-encoding*))
        (buf (first (pt-data-buffers pt))))
    (if (or (null buf) (> (+ strlen (data-buffer-size buf)) (data-buffer-capacity buf)))
        (let* ((new-capacity (max strlen +min-data-buffer-size+)) ; v harmless null for now
               (new-char* (ffi:foreign-alloc :unsigned-char :count new-capacity))
               (new-buffer (make-data-buffer :size strlen
                                             :capacity new-capacity
                                             :data new-char*
                                             :type :alloc)))
          (ffi:lisp-string-to-foreign string new-char*
                                      (1+ strlen) ;no. bytes including null
                                      :encoding ffi:*default-foreign-encoding*)
          (push new-buffer (pt-data-buffers pt))
          (values new-char* strlen))
        (let ((new-char* (ffi:lisp-string-to-foreign string (data-buffer-data buf)
                                                     (1+ strlen) ;no. bytes including null
                                                     :offset (data-buffer-size buf)
                                                     :encoding ffi:*default-foreign-encoding*)))
          (incf (data-buffer-size buf) strlen)
          (values new-char* strlen)))))

(defun data-buffer-free (data-buffer)
  (case (data-buffer-type data-buffer)
    (:alloc (ffi:foreign-string-free (data-buffer-data data-buffer)))
    (:mmap)))

(defstruct (piece (:print-object
                   (lambda (piece stream)
                     (let* ((enc::*suppress-character-coding-errors* t)
                            (contents
                              (ffi:foreign-string-to-lisp (piece-data piece)
                                                          :count (piece-size piece)
                                                          :max-chars 100)))
                       (format stream "#S(piece object with size:~d contains:~a~@[...~])"
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
  (tracked-cursors (list) :type list)
  ;; must be held around accesses to the buffer's pieces/data-buffers/tracked-cursors to
  ;; safeguard the the integrity of the tracked-cursors list and also the correctness
  ;; of cursors - i.e. MAKE-CURSOR
  (lock nil :type t)
  (revision 0 :type idx))

;; (defvar *lock-piece-table* t
;;   "Can be dynamically bound in individual threads to prevent locking (you can/need to do
;; the locking yourself).
;; IMPORTANT: if you dynamically bind this to NIL and are accessing multiple buffers, be
;; SURE to lock each one on your first access, then unlock afterwards before moving on.")

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
  (declare (ignore initial-stream))
  ;;(let ((static-vectors:make-static-vector)))
  (multiple-value-bind (init-char* init-length)
      ;;(ffi:foreign-string-alloc initial-contents :null-terminated-p t)
      (ffi:foreign-alloc :unsigned-char :count (1+ (length initial-contents))
                                        :initial-contents initial-contents)
    (setf (ffi:mem-ref init-char* :unsigned-char (length initial-contents)) 0)
    (setf init-length (length initial-contents))
    ;;(decf init-length) ;TODO null terminator variable length, encodings
    (let* ((init-buffer (make-data-buffer :size init-length
                                          :capacity init-length
                                          :data init-char*))
           (init-piece (make-piece :size init-length :data init-char*))
           (pt (make-piece-table :size init-length
                                 :data-buffers (list init-buffer))))
      (setf (piece-next (pt-sentinel-start pt)) init-piece
            (piece-prev init-piece) (pt-sentinel-start pt)
            (piece-prev (pt-sentinel-end pt)) init-piece
            (piece-next init-piece) (pt-sentinel-end pt))
      pt)))

(defmethod buf:copy-buffer ((pt piece-table))
  ;;(copy-piece-table pt) ;TODO deep copy required, be careful
  (error "not yet implemented"))

(defmethod buf:close-buffer ((pt piece-table)) ;TODO munmap
  (unless (pt-closed-p pt)
    ;; Here we invalidate all cursors, meaning no more future accesses to the buffer,
    ;; but it will be safe until other threads finish. Once cursors finish, references
    ;; to them should be dropped and the finalizer will run (eventually).
    ;; TODO document that cursor-invalid may be due to buffer being closed, meaning
    ;; cursors referencing the buffer should be un-referenced
    (with-pt-lock (pt)
      (setf (pt-closed-p pt) t)
      ;; revision update must happen after closing for MAKE-CURSOR to ensure eventual
      ;; invalidation - we lock just in case (if they don't function as memory barriers)
      (incf (pt-revision pt)))
    (let ((data-buffers (pt-data-buffers pt)))
      (tg:finalize pt (lambda () (mapcar #'data-buffer-free data-buffers))))))

(defmethod buf:size ((pt piece-table))
  (pt-size pt))

(declaim (ftype (function (t idx idx) idx) count-lfs))
(defun count-lfs (piece start end)
  "note: accesses foreign memory, must be locked"
  (declare (optimize speed (safety 0))
           (type idx start end))
  (loop :with lfs :of-type idx
        :with len :of-type idx = (- end start)
        :with ptr = (inc-ptr (piece-data piece) start)
        :do (let ((found (ffi:foreign-funcall "memchr"
                                              :pointer ptr
                                              :int #.(char-code #\newline)
                                              :long len
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

;; TODO cache line numbers for people who like line numbering
(defstruct (cursor (:copier %copy-cursor)
                   (:print-object
                    (lambda (cursor stream)
                      (format stream "#S(cursor with~@
                                         piece:~a~%byte-offset:~d~@
                                         lock:~a~%revision:~d)"
                              (cursor-piece cursor)
                              (cursor-byte-offset cursor)
                              (cursor-lock cursor)
                              (cursor-revision cursor)))))
  (piece-table nil :read-only t)
  (tracked-p nil :type boolean) ;if T, we can always access
  piece
  (byte-offset 0 :type idx)
  (lock nil :type t) ; used to safeguard COPY-CURSOR
  (revision 0 :type idx))

(defun copy-cursor (cursor)
  (let ((copy (with-cursor-lock (cursor) (%copy-cursor cursor))))
    (setf (cursor-lock copy) nil
          (cursor-tracked-p copy) nil)
    copy))

(defmethod buf:make-cursor ((pt piece-table) index)
  (with-pt-lock (pt)
    (loop :with revision = (if (pt-closed-p pt)
                               -1 ; invalidated immediately
                               (pt-revision pt))
          :with piece-index :of-type idx
          :for piece = (piece-next (pt-sentinel-start pt)) :then (piece-next piece)
          :while (piece-next piece)
          :when (<= piece-index index (+ piece-index (1- (piece-size piece))))
            :return (let ((byte-offset (- index piece-index)))
                      (make-cursor :piece-table pt
                                   :piece piece
                                   :byte-offset byte-offset
                                   :revision revision))
          :do (incf piece-index (piece-size piece))
          :finally (when (= piece-index index) ;off-end
                     (return (make-cursor :piece-table pt
                                          :piece (piece-prev piece)
                                          :byte-offset (piece-size (piece-prev piece))
                                          :revision revision)))
                   (error 'conditions:vico-bad-index
                          :buffer pt
                          :bad-index index
                          :bounds (cons 0 (pt-size (cursor-piece-table cursor)))))))

(defmethod buf:copy-cursor ((cursor cursor))
  (check-cursor cursor)
  (copy-cursor cursor))

(defun %blit-cursor (dest src) ;TODO
  "CAREFUL: very dangerous. Only use if SRC is a fresh local copy and DEST lock is held"
  (setf (cursor-byte-offset dest) (cursor-byte-offset src)
        (cursor-piece dest) (cursor-piece src)))

(defmethod buf:cursor-buffer ((cursor cursor)) ;unlocked
  (check-cursor cursor)
  (cursor-piece-table cursor))

(defun check-cursor (cursor)
  (declare (optimize speed (safety 0)))
  (let ((pt (cursor-piece-table cursor)))
    (or (= (cursor-revision cursor) (pt-revision pt))
        (error 'conditions:vico-cursor-invalid :cursor cursor :buffer pt))))

(defmethod buf:track-cursor ((cursor cursor))
  (with-pt-lock ((cursor-piece-table cursor))
    (check-cursor cursor)
    (setf (cursor-tracked-p cursor) t) ; irrelevant in COPY-CURSOR, no locking needed
    (push cursor (pt-tracked-cursors (cursor-piece-table cursor)))
    cursor))

(defmethod buf:untrack-cursor ((cursor cursor))
  (with-pt-lock ((cursor-piece-table cursor))
    (check-cursor cursor)
    (setf (cursor-tracked-p cursor) nil) ; same here
    (removef (pt-tracked-cursors (cursor-piece-table cursor)) cursor)
    cursor))

;; readers

(defmethod buf:index-at ((cursor cursor))
  (check-cursor cursor)
  (loop :with index = (cursor-byte-offset cursor)
        :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
        :while (piece-prev piece)
        :do (incf index (piece-size piece))
        :finally (return index)))

(defmethod buf:line-at ((cursor cursor))
  (declare (optimize speed))
  (check-cursor cursor)
  (loop :with line :of-type idx
          = (count-lfs (cursor-piece cursor) 0 (cursor-byte-offset cursor))
        :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
        :while (piece-prev piece)
        :do (incf line (count-lfs piece 0 (piece-size piece)))
        :finally (return (1+ line))))

(defmethod buf:byte-at ((cursor cursor))
  (check-cursor cursor)
  (ffi:mem-ref (inc-ptr (piece-data (cursor-piece cursor)) (cursor-byte-offset cursor))
               :unsigned-char))

(declaim (inline utf8-char-at))
(defun utf8-char-at (octets max)
  (declare (type ffi:foreign-pointer octets)
           (type idx max))
  (let* ((leading-byte (ffi:mem-ref octets :unsigned-char))
         (char-byte-size (cond ((< leading-byte #x80) 1)
                               ((< leading-byte #xE0) 2)
                               ((< leading-byte #xF0) 3)
                               ((< leading-byte #xF8) 4)))
         (codepoint (if (= char-byte-size 1)
                        leading-byte
                        (logand leading-byte (ecase char-byte-size
                                               (2 #x1F)
                                               (3 #x0F)
                                               (4 #x07))))))
    (declare (type (integer 0 #x10ffff) codepoint))
    (loop :initially (or (<= char-byte-size max) (return (code-char #xfffd)))
          :for i :from 1 :below char-byte-size
          :do (setf codepoint (logior (ash codepoint 6)
                                      (logand (ffi:mem-ref octets :unsigned-char i) #x3F)))
          :finally (return (code-char codepoint)))))

(defun char-at (cursor)
  (declare (optimize speed (safety 0)))
  (let ((piece (cursor-piece cursor)))
    (utf8-char-at (inc-ptr (piece-data piece) (cursor-byte-offset cursor))
                  (- (piece-size piece) (cursor-byte-offset cursor)))))

(defmethod buf:char-at ((cursor cursor)) ;TODO should error for off-end
  (check-cursor cursor)
  (char-at cursor))

;; movement

;; note to self: the IF branches are not necessary, but are possibly clearer and more
;; efficient (likely branch, less+) - reflects byte-offset = 0 in subsequent iterations

(defun cursor-next (cursor count)
  (declare (optimize speed (safety 0))
           (type idx count))
  (if (< (+ (cursor-byte-offset cursor) count) (piece-size (cursor-piece cursor)))
      (incf (cursor-byte-offset cursor) count)
      (loop :initially (decf left (- (piece-size (cursor-piece cursor))
                                     (cursor-byte-offset cursor)))
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
                           (t
                            (error 'conditions:vico-bad-index
                                   :buffer (cursor-piece-table cursor)
                                   :bad-index (+ (buf:index-at cursor) count)
                                   :bounds (cons 0 (buf:size (cursor-piece-table cursor)))))))))

(defmethod buf:cursor-next ((cursor cursor) &optional (count 1))
  (check-cursor cursor)
  (with-cursor-lock (cursor)
    (cursor-next cursor count))
  cursor)

;; same as above, but byte-offset = piece-size in subsequent iterations

(defun cursor-prev (cursor count)
  (declare (optimize speed (safety 0))
           (type idx count))
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
                         (error 'conditions:vico-bad-index
                                :buffer (cursor-piece-table cursor)
                                :bad-index (- (buf:index-at cursor) count)
                                :bounds (cons 0 (buf:size (cursor-piece-table cursor))))))))

(defmethod buf:cursor-prev ((cursor cursor) &optional (count 1))
  (check-cursor cursor)
  (with-cursor-lock (cursor)
    (cursor-prev cursor count))
  cursor)

(defun skip-lfs (piece) ;TODO
  piece)

;;  0     1         2      3 4
;; .\nx...\n....|...\n.....\nx.|...
;; 3  ^                         ^

(defun cursor-next-line (cursor count)
  (declare (optimize speed)
           (type idx count))
  (loop :with copy = (%copy-cursor cursor)
        :for piece = (cursor-piece copy) :then (piece-next piece)
        :for start = (inc-ptr (piece-data piece) (cursor-byte-offset copy))
        :do (loop :with found :of-type ffi:foreign-pointer
                  :for piece-end :of-type idx
                    = (+ (the idx (ffi:pointer-address (piece-data piece)))
                         (piece-size piece))
                  :for n :of-type idx = (- piece-end (the idx (ffi:pointer-address start)))
                  :while (and (plusp count) (plusp n))
                  :do (setf found (ffi:foreign-funcall "memchr"
                                                       :pointer start
                                                       :int #.(char-code #\newline)
                                                       :long n
                                                       :pointer))
                      (when (ffi:null-pointer-p found)
                        (cursor-next copy n)
                        (return))
                      (cursor-next copy (1+ (- (the idx (ffi:pointer-address found))
                                               (the idx (ffi:pointer-address start)))))
                      (setf start (inc-ptr found 1))
                      (decf count))
        :while (and (plusp count) (piece-next piece)) ;not sentinel
        :finally (or (piece-next piece)
                     (error 'conditions:vico-bad-line-number
                            :buffer (cursor-piece-table cursor)
                            :line-number (+ (buf:line-at cursor) count)
                            :bounds (cons 1 (1+ (buf:line-count (cursor-piece-table cursor))))))
                 (%blit-cursor cursor copy)))

(defmethod buf:cursor-next-line ((cursor cursor) &optional (count 1))
  (check-cursor cursor)
  (with-cursor-lock (cursor)
    (cursor-next-line cursor count))
  cursor)

(defun cursor-prev-line (cursor count)
  (declare (optimize speed)
           (type idx count))
  (unless (zerop count)
    (loop :with needed-count = (1+ count) ; jump backwards count+1, step forward once
          :with copy = (%copy-cursor cursor)
          :for piece = (cursor-piece copy) :then (piece-prev piece)
          :for start-offset :of-type idx = (cursor-byte-offset copy)
            :then (piece-size piece)
          :do (loop :with found :of-type ffi:foreign-pointer
                    :for start-ptr = (piece-data piece)
                    :while (and (plusp needed-count) (plusp start-offset))
                    :do (setf found (ffi:foreign-funcall "memrchr"
                                                         :pointer start-ptr
                                                         :int #.(char-code #\newline)
                                                         :long start-offset
                                                         :pointer))
                        (when (ffi:null-pointer-p found) ; move back a piece
                          (cursor-prev copy start-offset)
                          (return))
                        (cursor-prev copy (- (the idx (+ (the idx (ffi:pointer-address start-ptr))
                                                         start-offset))
                                             (the idx (ffi:pointer-address found))))
                        (setf start-offset (- (the idx (ffi:pointer-address found))
                                              (the idx (ffi:pointer-address start-ptr))))
                        (decf needed-count))
          :while (and (plusp needed-count) (piece-prev piece)) ;not sentinel
          :finally (cond ((piece-prev piece)
                          (cursor-next copy 1)
                          (%blit-cursor cursor copy))
                         ((= needed-count 1)
                          (assert (eq piece (pt-sentinel-start (cursor-piece-table cursor))))
                          (setf (cursor-piece copy) (piece-next piece)
                                (cursor-byte-offset copy) 0)
                          (%blit-cursor cursor copy))
                         (t
                          (error 'conditions:vico-bad-line-number
                                 :buffer (cursor-piece-table cursor)
                                 :line-number (- (buf:line-at cursor) count)
                                 :bounds (cons 1 (1+ (buf:line-count (cursor-piece-table cursor))))))))))

(defmethod buf:cursor-prev-line ((cursor cursor) &optional (count 1))
  (check-cursor cursor)
  (with-cursor-lock (cursor)
    (cursor-prev-line cursor count))
  cursor)

;; TODO support arbitrary encodings, memoize mappings in struct. assuming UTF-8 for now
;; PR for babel to have leading-byte test
;; TODO restarts for bounds overrun of char/find/search - untracked text properties
;; remain-at-end/start, revert-movement

(defun cursor-next-char (cursor count)
  (declare (optimize speed (safety 0))
           (type idx count))
  (loop :repeat count
        :do (loop
              (with-cursor-lock (cursor)
                (cursor-next cursor 1))
              (let ((byte (ffi:mem-ref (piece-data (cursor-piece cursor)) :unsigned-char
                                       (cursor-byte-offset cursor))))
                (or (= (logand byte #xC0) #x80)
                    (return))))))

(defmethod buf:cursor-next-char ((cursor cursor) &optional (count 1))
  (check-cursor cursor)
  (cursor-next-char cursor count)
  cursor)

(defun cursor-prev-char (cursor count)
  (declare (optimize speed)
           (type idx count))
  (loop :repeat count
        :do (loop
              (with-cursor-lock (cursor)
                (cursor-prev cursor 1))
              (let ((byte (ffi:mem-ref (piece-data (cursor-piece cursor)) :unsigned-char
                                       (cursor-byte-offset cursor))))
                (or (= (logand byte #xC0) #x80)
                    (return))))))

(defmethod buf:cursor-prev-char ((cursor cursor) &optional (count 1))
  (check-cursor cursor)
  (cursor-prev-char cursor count)
  cursor)

(defmethod buf:cursor-find-next ((cursor cursor) char)
  (check-cursor cursor)
  (handler-case
      (loop :until (char= (buf:char-at cursor) char) ;pt-lock acquisition
            :do (cursor-next-char cursor 1)
            :finally (return (values char cursor)))
    (conditions:vico-bad-index () ;overrun, cursor is at end of buffer
      (values nil cursor))))

(defmethod buf:cursor-find-prev ((cursor cursor) char)
  (check-cursor cursor)
  (handler-case
      (loop :do (cursor-prev-char cursor 1)
            :until (char= (buf:char-at cursor) char) ;pt-lock acquisition
            :finally (return (values char cursor)))
    (conditions:vico-bad-index () ;cursor at start
      (values nil cursor))))

;; TODO use cursor-prev-char and index tracking closure
(defmethod buf:cursor-search-next ((cursor cursor) string)
  (check-cursor cursor)
  string)

(defmethod buf:cursor-search-prev ((cursor cursor) string)
  (check-cursor cursor)
  string)

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

;;(pt-string (first (vico-core.evloop:buffers vico-core.evloop:*editor*)))
;; (defparameter test
;;   (buf:make-cursor (first (vico-core.evloop:buffers vico-core.evloop:*editor*))
;;                    10))
;;(buf:cursor-next-line test)
;; (buf:subseq-at test 20)
;; (finish-output)

;; (time
;;  (let ((piece (piece-next
;;                (pt-sentinel-start
;;                 (first (vico-core.evloop:buffers vico-core.evloop:*editor*))))))
;;    (count-lfs piece 0 (piece-size piece))))

(defmethod buf:insert-at ((cursor cursor) string)
  (check-cursor cursor)
  (unless (zerop (length string))
    (let* ((pt (cursor-piece-table cursor))
           (tracked-cursors (pt-tracked-cursors pt))
           (piece (cursor-piece cursor))
           (ptr (piece-data piece))
           (prev (piece-prev (cursor-piece cursor))))
      (setf (pt-line-cache-valid-p pt) nil) ; race, race, race!
      (with-pt-lock (pt)
        ;; commit revision: this will not save in-progress reads, but early is good
        ;; we care to lock this access to prevent MAKE-CURSOR returning
        ;; (actually) invalidated cursors
        (incf (pt-revision pt))
        (unwind-protect
             (progn
               (or (cursor-tracked-p cursor) (lock-spinlock (cursor-lock cursor)))
               (mapcar #'(lambda (cursor)
                           (lock-spinlock (cursor-lock cursor)))
                       tracked-cursors)
               (multiple-value-bind (new-ptr strlen)
                   (data-buffer-insert pt string)
                 (incf (pt-size pt) strlen)
                 (cond ((zerop (cursor-byte-offset cursor)) ;boundary case
                        ;; TODO optimize appending using END-CACHE
                        (let ((new-piece (make-piece :prev prev :next piece
                                                     :data new-ptr
                                                     :size strlen)))
                          (setf (piece-next prev) new-piece
                                (piece-prev piece) new-piece)))
                       ((< (cursor-byte-offset cursor) (piece-size piece))
                        (let* ((next (piece-next piece))
                               (new-piece (make-piece :data new-ptr
                                                      :size strlen))
                               (left-split
                                 (make-piece :prev prev :next new-piece
                                             :data ptr
                                             :size (cursor-byte-offset cursor)))
                               (right-split
                                 (make-piece :prev new-piece :next next
                                             :data (inc-ptr ptr
                                                            (cursor-byte-offset cursor))
                                             :size (- (piece-size piece)
                                                      (cursor-byte-offset cursor)))))
                          (mapcar #'(lambda (tcursor)
                                      (when (and (eq (cursor-piece tcursor) piece)
                                                 (not (eq tcursor cursor)))
                                        (if (>= (cursor-byte-offset tcursor)
                                                (cursor-byte-offset cursor))
                                            (progn
                                              (setf (cursor-piece tcursor) right-split)
                                              (decf (cursor-byte-offset tcursor)
                                                    (cursor-byte-offset cursor)))
                                            (setf (cursor-piece tcursor) left-split))))
                                  tracked-cursors)
                          (setf (piece-prev new-piece) left-split
                                (piece-next new-piece) right-split
                                (piece-next prev) left-split
                                (piece-prev next) right-split)
                          (setf (cursor-piece cursor) right-split
                                (cursor-byte-offset cursor) 0)))
                       (t ; off-end
                        (let* ((sentinel (piece-next piece))
                               (new-piece (make-piece :prev piece :next sentinel
                                                      :data new-ptr
                                                      :size strlen)))
                          (setf (piece-next piece) new-piece
                                (piece-prev sentinel) new-piece)
                          (setf (cursor-piece cursor) new-piece
                                (cursor-byte-offset cursor) (piece-size new-piece)))))))
          (mapcar #'(lambda (cursor)
                      (incf (cursor-revision cursor))
                      (unlock-spinlock (cursor-lock cursor)))
                  tracked-cursors)
          (or (cursor-tracked-p cursor)
              (progn
                (incf (cursor-revision cursor))
                (unlock-spinlock (cursor-lock cursor)))))) ;pt-lock & unwind-protect end
      pt)))

(defmethod buf:erase-at ((cursor cursor) &optional count)
  (check-cursor cursor)
  (let ((pt (cursor-piece-table cursor)))
    (with-pt-lock (pt)
      (incf (pt-revision pt))
      (setf (pt-line-cache-valid-p pt) nil)
      count)))

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
          :do (setf string (concatenate 'string string
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
;;                                   (princ (buf:char-at c) s)
;;                                   (princ (buf:line-at c) s))
;;                               (enc:character-decoding-error ()))))
;;                  (format nil "t1e1s1t1~%1b2α2f2~%2")))
