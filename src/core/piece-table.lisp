;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; piece table rewrite
;;
;; DONE implement interface
;; TODO mmap()
;; TODO error hanglig
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

(defconstant +min-data-buffer-size+ (expt 2 12)
  "the page size on most x64 platforms?")

(defstruct (data-buffer (:print-object
                         (lambda (db stream)
                           (format stream "#S(size:~d capacity:~d ~
                                              ptr ~a type ~s)"
                                   (data-buffer-size db)
                                   (data-buffer-capacity db)
                                   (data-buffer-data db)
                                   (data-buffer-type db)))))
  "manages a pointer to a foreign vector of octets"
  (size 0 :type idx)
  (capacity 0 :type idx :read-only t)
  (data (ffi:null-pointer) :type ffi:foreign-pointer :read-only t)
  (type :alloc :type (member :mmap :alloc) :read-only t)
  (lf-vec (make-array 0 :element-type 'idx :fill-pointer t :adjustable t)
   :type (vector idx)))

(defstruct (piece (:print-object
                   (lambda (piece stream)
                     (format stream "#S(size:~d lf-offset:~d contains:~a)"
                             (piece-size piece)
                             (piece-lf-offset piece)
                             (ffi:foreign-string-to-lisp (piece-data piece)
                                                         :count (piece-size piece)
                                                         :max-chars 100)))))
  "SIZE is in octets"
  prev next
  (data (ffi:null-pointer) :type ffi:foreign-pointer)
  (size 0 :type idx)
  (lfs 0 :type idx)
  (lf-offset 0 :type idx)
  (lf-vec (make-array 0 :element-type 'idx :fill-pointer t :adjustable t)
   :type (vector idx)
   :read-only t))

(defstruct (piece-table (:conc-name pt-))
  (size 0 :type idx)
  (line-count 0 :type idx)
  (data-buffers (list) :type list)
  (end-cache nil)
  sentinel-start sentinel-end
  (closed-p nil)
  ;; TODO maybe sorted - order kept during insertions, so we can update with local info
  (tracked (list) :type list)
  ;; must be held around access, use some other mechanism later if performance is an issue
  ;; safeguards MAKE-CURSOR and CLOSE-BUFFER
  (lock (bt:make-lock "piece-table lock") :type t)
  (revision 0 :type idx))

(defvar *lock-piece-table* t
  "Can be dynamically bound in individual threads to prevent locking (you can/need to do
the locking yourself).
IMPORTANT: if you dynamically bind this to NIL and are accessing multiple buffers, be
SURE to lock each one on your first access, then unlock afterwards before moving on.")

;;(loop :until (atomics:cas (pt-lock ,piece-table) t nil))
(defmacro with-pt-lock ((piece-table) &body body)
  `(if *lock-piece-table*
       (bt:with-lock-held ((pt-lock ,piece-table))
         ,@body)
       (progn ,@body)))

(defun push-lfs-to-vector (ptr ptr-length ptr-offset vector)
  "PTR is foreign memory, PTR-LENGTH will be scanned for linefeeds, which will be pushed
onto VECTOR. PTR-OFFSET is summed to found offsets."
  (declare (optimize speed)
           (type ffi:foreign-pointer ptr)
           (type idx ptr-length ptr-offset))
  (loop :for index :below ptr-length
        :with count :of-type idx = 0
        :do (when (= (ffi:mem-ref ptr :char) #.(char-code #\newline))
              (incf count)
              (vector-push-extend (the idx (+ index ptr-offset)) vector))
            (ffi:incf-pointer ptr)
        :finally (return count)))

(defmethod buf:make-buffer ((type (eql :piece-table)) &key (initial-contents "")
                                                        initial-stream)
  (declare (ignore initial-stream))
  (multiple-value-bind (init-char* init-length)
      (ffi:foreign-string-alloc initial-contents :null-terminated-p t) ;TODO remove later
    (let ((init-lfs (make-array 0 :element-type 'idx :fill-pointer t :adjustable t)))
      (decf init-length) ;TODO null terminator variable length
      (push-lfs-to-vector init-char* init-length 0 init-lfs)
      (let* ((init-buffer (make-data-buffer :size init-length
                                            :capacity init-length
                                            :data init-char*
                                            :lf-vec init-lfs))
             (sentinel-start (make-piece))
             (sentinel-end (make-piece))
             (init-piece (make-piece :prev sentinel-start :next sentinel-end
                                     :size init-length
                                     :data init-char*
                                     :lf-offset 0
                                     :lfs (fill-pointer init-lfs)
                                     :lf-vec init-lfs))
             (pt (make-piece-table :size init-length
                                   :line-count (fill-pointer init-lfs)
                                   :sentinel-start sentinel-start
                                   :sentinel-end sentinel-end
                                   :data-buffers (list init-buffer))))
        (setf (piece-next sentinel-start) init-piece
              (piece-prev sentinel-end) init-piece)
        pt))))

(defmethod buf:close-buffer ((pt piece-table)) ;TODO munmap
  (unless (pt-closed-p pt)
    (with-pt-lock (pt)
      (loop :initially (setf (pt-closed-p pt) t)
            :for buffer :in (pt-data-buffers pt)
            :do (ffi:foreign-free (data-buffer-data buffer))))))

(defmethod buf:size ((pt piece-table))
  (pt-size pt))

(defmethod buf:line-count ((pt piece-table))
  (pt-line-count pt))

;; TODO optimize, switch to (simple-array idx)
(defun piece-byte->lf-offset (piece offset)
  (declare (optimize speed)
           (type idx offset))
  (loop :for lf-overshoot-idx :of-type idx :from (piece-lf-offset piece)
          :below (+ (piece-lf-offset piece) (piece-lfs piece))
        :when (>= (aref (piece-lf-vec piece) lf-overshoot-idx) offset)
          :return (the idx (- lf-overshoot-idx (piece-lf-offset piece)))
        :finally (return (piece-lfs piece))))


(defparameter *pt* (buf:make-buffer :piece-table
                                    :initial-contents "test
bαf
"))

;;; cursor

;; regarding thread-safety - non-evloop-thread cursor access during editing:
;; modifying an invalid cursor's numeric fields is safe
;; at no point are cursor-prev, cursor-next (eql NIL) (maintain), so those are safe
;; accessing the piece data buffer or lf-vec in byte->lf is safe, append-only

;; cursors themselves are owned by a single thread and so no threading issues should exist,
;; except with COPY-CURSOR, which locks and is thus thread-safe
;; CURSOR-NEXT(-LINE), CURSOR-PREV(-LINE) lock as they mutate the cursor

;; n.b. when applicable, lock order is strictly pt-lock, cursor-lock

(defmacro with-cursor-lock ((cursor) &body body)
  "MUST be used around during cursor mutations"
  `(bt:with-lock-held ((cursor-lock ,cursor))
     ,@body))

(defstruct (cursor (:copier %copy-cursor))
  (piece-table nil :read-only t)
  (tracked-p nil :type boolean) ;if T, we can always access
  piece
  (byte-offset 0 :type idx)
  (lf-offset 0 :type idx)
  (lock (bt:make-lock "cursor lock") :type t) ; used to safeguard COPY-CURSOR
  (revision 0 :type idx))

(defun copy-cursor (cursor)
  (let ((copy (with-cursor-lock (cursor) (%copy-cursor cursor))))
    (setf (cursor-lock copy) (bt:make-lock "cursor copy lock")
          (cursor-tracked-p copy) nil)
    copy))

(defmacro with-cursor-abort-on-pt-close ((cursor) &body body)
  "MUST be used around accesses to the cursor's data, which is freed by CLOSE-BUFFER"
  `(with-pt-lock ((cursor-piece-table ,cursor))
     (if (pt-closed-p (cursor-piece-table cursor))
         (error 'conditions:vico-cursor-invalid :cursor cursor)
         (progn ,@body))))

(defmethod buf:make-cursor ((pt piece-table) index)
  (with-pt-lock (pt)
    (loop :with piece-index :of-type idx
          :with lf-index :of-type idx
          :for piece = (piece-next (pt-sentinel-start pt)) :then (piece-next piece)
          :while (piece-next piece)
          :when (<= piece-index index (+ piece-index (1- (piece-size piece))))
            :return (let ((byte-offset (- index piece-index)))
                      (make-cursor :piece-table pt
                                   :piece piece
                                   :byte-offset byte-offset
                                   :lf-offset (+ (piece-byte->lf-offset piece byte-offset)
                                                 lf-index)))
          :do (incf piece-index (piece-size piece))
              (incf lf-index (piece-lfs piece))
          :finally (when (= piece-index index) ;off-end
                     (return (make-cursor :piece-table pt
                                          :piece (piece-prev piece)
                                          :byte-offset (piece-size (piece-prev piece))
                                          :lf-offset (+ lf-index (piece-lfs piece))))))))

(defmethod buf:copy-cursor ((cursor cursor))
  (copy-cursor cursor))

(defmethod buf:cursor-buffer ((cursor cursor)) ;unlocked
  (cursor-piece-table cursor))

(defmethod buf:track-cursor ((cursor cursor))
  (setf (cursor-tracked-p cursor) t) ; irrelevant in COPY-CURSOR, no locking needed
  (with-pt-lock ((cursor-piece-table cursor))
    (push cursor (pt-tracked (cursor-piece-table cursor)))))

(defmethod buf:untrack-cursor ((cursor cursor))
  (setf (cursor-tracked-p cursor) nil) ; same here
  (with-pt-lock ((cursor-piece-table cursor))
    (removef (pt-tracked (cursor-piece-table cursor)) cursor)))

;; readers

(defmethod buf:index-at ((cursor cursor))
  (loop :with index = (cursor-byte-offset cursor)
        :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
        :while (piece-prev piece) ;sentinels must have 0 size
        :do (incf index (piece-size piece))
        :finally (return index)))

(defmethod buf:line-at ((cursor cursor))
  (loop :with lf-index = (cursor-lf-offset cursor)
        :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
        :while (piece-prev piece)
        :do (incf lf-index (piece-lfs piece))
        :finally (return (1+ lf-index))))

(defmethod buf:byte-at ((cursor cursor))
  (with-cursor-abort-on-pt-close (cursor)
    (ffi:mem-ref (ffi:inc-pointer (piece-data (cursor-piece cursor))
                                  (cursor-byte-offset cursor))
                 :char)))

(defmethod buf:char-at ((cursor cursor))
  (when (= (cursor-byte-offset cursor) (piece-size (cursor-piece cursor))) ;eob
    (error 'conditions:vico-bad-index
           :buffer (cursor-piece-table cursor)
           :bad-index (+ (pt-size (cursor-piece-table cursor)) 1)
           :bounds (cons 0 (1- (pt-size (cursor-piece-table cursor))))))
  (schar
   (with-cursor-abort-on-pt-close (cursor)
     (ffi:foreign-string-to-lisp (ffi:inc-pointer (piece-data (cursor-piece cursor))
                                                  (cursor-byte-offset cursor))
                                 :count 4
                                 :max-chars 1))
   0))

;; movement

;; TODO searching - implement bol also
;; TODO restarts for bounds overrun: remain-at-end/start, revert-movement

(defun cursor-next-within-piece (cursor count)
  (setf (cursor-lf-offset cursor) ;TODO unneeded lf scan
        (piece-byte->lf-offset (cursor-piece cursor)
                               (incf (cursor-byte-offset cursor) count))))

;; note to self: the IF branches are not necessary, but are possibly clearer and more
;; efficient (likely branch, less+) - reflects byte/lf-offset = 0 in subsequent iterations

(defun cursor-next (cursor count)
  (if (< (+ (cursor-byte-offset cursor) count) (piece-size (cursor-piece cursor)))
      (cursor-next-within-piece cursor count)
      (loop :initially (decf count (- (piece-size (cursor-piece cursor))
                                      (cursor-byte-offset cursor)))
            :for piece = (piece-next (cursor-piece cursor)) :then (piece-next piece)
            :while (and (>= count (piece-size piece)) (piece-next piece))
            :do (decf count (piece-size piece))
            :finally (cond ((piece-next piece)
                            (setf (cursor-piece cursor) piece)
                            (cursor-next-within-piece cursor count))
                           ((zerop count) ;end-of-buffer
                            (setf (cursor-piece cursor) (piece-prev piece)
                                  (cursor-byte-offset cursor) (piece-size
                                                               (cursor-piece cursor))
                                  (cursor-lf-offset cursor) (piece-lfs
                                                             (cursor-piece cursor))))
                           (t
                            (error 'conditions:vico-bad-index
                                   :buffer (cursor-piece-table cursor)
                                   :bad-index (+ (buf:index-at cursor) count)
                                   :bounds (cons 0 (1- (pt-size
                                                        (cursor-piece-table cursor)))))))))
  cursor)

(defmethod buf:cursor-next ((cursor cursor) &optional (count 1))
  (with-cursor-lock (cursor)
    (cursor-next cursor count)))

(defun cursor-prev-within-piece (cursor count)
  (setf (cursor-lf-offset cursor) ;TODO unneeded lf scan
        (piece-byte->lf-offset (cursor-piece cursor)
                               (decf (cursor-byte-offset cursor) count))))

;; same as above, but byte/lf-offset = (piece-size/lfs) in subsequent iterations

(declaim (notinline cursor-prev))
(defun cursor-prev (cursor count)
  (if (>= (cursor-byte-offset cursor) count)
      (cursor-prev-within-piece cursor count)
      (loop :initially (decf count (cursor-byte-offset cursor))
            :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
            :while (and (> count (piece-size piece)) (piece-prev piece))
            :do (decf count (piece-size piece))
            :finally (if (piece-prev piece)
                         (progn
                           (setf (cursor-piece cursor) piece
                                 (cursor-byte-offset cursor) (piece-size piece)
                                 (cursor-lf-offset cursor) (piece-lfs piece))
                           (cursor-prev-within-piece cursor count))
                         (error 'conditions:vico-bad-index
                                :buffer (cursor-piece-table cursor)
                                :bad-index (- (buf:index-at cursor) count)
                                :bounds (cons 0 (1- (pt-size
                                                     (cursor-piece-table cursor))))))))
  cursor)

(defmethod buf:cursor-prev ((cursor cursor) &optional (count 1))
  (with-cursor-lock (cursor)
    (cursor-prev cursor count)))

(defun cursor-next-line-within-piece (cursor count)
  "found lf is, but line is not necessarily within - (cursor-next) may step past bounds"
  (setf (cursor-byte-offset cursor) ; jump to eol of (1- count) lines forward
        (aref (piece-lf-vec (cursor-piece cursor))
              (incf (cursor-lf-offset cursor) (1- count))))
  (cursor-next cursor 1)) ; step forward once over lf

;;  0     1         2      3 4
;; .\nx...\n....|...\n.....\nx.|...
;; 3  ^                         ^

(defun cursor-next-line (cursor count)
  (unless (zerop count)
    (if (<= (+ (cursor-lf-offset cursor) count) (piece-lfs (cursor-piece cursor)))
        (cursor-next-line-within-piece cursor count)
        (loop :initially (decf count (- (piece-lfs (cursor-piece cursor))
                                        (cursor-lf-offset cursor)))
              :for piece = (piece-next (cursor-piece cursor)) :then (piece-next piece)
              :while (and (> count (piece-lfs piece)) (piece-next piece))
              :do (decf count (piece-lfs piece))
              :finally (if (piece-next piece)
                           (progn
                             (setf (cursor-piece cursor) piece)
                             (cursor-next-line-within-piece cursor count))
                           (error 'conditions:vico-bad-line-number
                                  :buffer (cursor-piece-table cursor)
                                  :line-number (+ (buf:line-at cursor) count)
                                  :bounds (cons 1 (pt-line-count
                                                   (cursor-piece-table cursor)))))))))

(defmethod buf:cursor-next-line ((cursor cursor) &optional (count 1))
  (with-cursor-lock (cursor)
    (cursor-next-line cursor count))
  cursor)

(defun cursor-prev-line-within-piece (cursor count)
  "line is strictly within - will not be called when line start is before first lf
n.b. this in particular means this will NOT be called to reach first line in buffer"
  (setf (cursor-byte-offset cursor) ; jump backwards (1+ count), step forward 1
        (aref (piece-lf-vec (cursor-piece cursor))
              (decf (cursor-lf-offset cursor) (1+ count))))
  (cursor-next cursor 1))

(defun cursor-prev-line (cursor count)
  (unless (zerop count)
    (if (> (cursor-lf-offset cursor) count)
        (cursor-prev-line-within-piece cursor count)
        (loop :initially (decf count (cursor-lf-offset cursor))
              :for piece = (piece-prev (cursor-piece cursor)) :then (piece-prev piece)
              :while (and (>= count (piece-lfs piece)) (piece-prev piece))
              :do (decf count (piece-lfs piece))
              :finally (cond ((piece-prev piece)
                              (setf (cursor-piece cursor) piece
                                    (cursor-byte-offset cursor) (piece-size piece)
                                    (cursor-lf-offset cursor) (piece-lfs piece))
                              (cursor-prev-line-within-piece cursor count))
                             ;; (not prev) - first line/index 0, no forward step here
                             ((zerop count)
                              (setf (cursor-piece cursor) (piece-next piece)
                                    (cursor-byte-offset cursor) 0
                                    (cursor-lf-offset cursor) 0))
                             (t
                              (error 'conditions:vico-bad-line-number
                                     :buffer (cursor-piece-table cursor)
                                     :line-number (- (buf:line-at cursor) count)
                                     :bounds (cons 1 (pt-line-count
                                                      (cursor-piece-table cursor))))))))))

(defmethod buf:cursor-prev-line ((cursor cursor) &optional (count 1))
  (with-cursor-lock (cursor)
    (cursor-prev-line cursor count))
  cursor)

(let ((counter (enc:code-point-counter
                (enc:lookup-mapping ffi::*foreign-string-mappings*
                                    ffi:*default-foreign-encoding*))))
  (declare (type function counter))
  (defun cursor-next-char (cursor count)
    (declare (optimize speed)
             (type idx count))
    (loop ;;:with max-units = (enc:enc-max-units-per-char
          ;;                  (enc:get-character-encoding ffi:*default-foreign-encoding*))
          ;; :with counter = (enc:code-point-counter
          ;;                  (enc:lookup-mapping ffi::*foreign-string-mappings*
          ;;                                      ffi:*default-foreign-encoding*))
          :with piece = (cursor-piece cursor)
          :repeat count
          :do (multiple-value-bind (chars step)
                  (with-cursor-abort-on-pt-close (cursor)
                    (funcall counter
                             (ffi:inc-pointer (piece-data piece) (cursor-byte-offset cursor))
                             0 ; offset 0
                             4 ; max-units
                             1)) ; 1 char max
                (declare (ignore chars)) ; = 1
                (with-cursor-lock (cursor)
                  (cursor-next cursor step))))))

;; TODO support arbitrary encodings
(defmethod buf:cursor-next-char ((cursor cursor) &optional (count 1))
  (cursor-next-char cursor count)
  cursor)

(defun cursor-prev-char (cursor count)
  (loop :with max-units = (enc:enc-max-units-per-char
                           (enc:get-character-encoding ffi:*default-foreign-encoding*))
        :with piece = (cursor-piece cursor)
        :repeat count
        :do (loop
              (with-cursor-lock (cursor) ;XXX kludge for backing up
                (cursor-prev cursor 1))
              (when (ignore-errors (ffi:foreign-string-to-lisp
                                    (ffi:inc-pointer (piece-data piece)
                                                     (cursor-byte-offset cursor))
                                    :count max-units
                                    :max-chars 1))
                (return)))))

(defmethod buf:cursor-prev-char ((cursor cursor) &optional (count 1))
  (cursor-prev-char cursor count)
  cursor)

(defmethod buf:cursor-find-next ((cursor cursor) char)
  (handler-case
      (loop :until (char= (buf:char-at cursor) char) ;pt-lock acquisition
            :do (cursor-next-char cursor 1)
            :finally (return (values char cursor)))
    (conditions:vico-bad-index (e) ;overrun, cursor is at end of buffer
      (declare (ignore e))
      (return-from buf:cursor-find-next (values nil cursor)))))

(defmethod buf:cursor-find-prev ((cursor cursor) char)
  (handler-case
      (loop :do (cursor-prev-char cursor 1)
            :until (char= (buf:char-at cursor) char) ;pt-lock acquisition
            :finally (return (values char cursor)))
    (conditions:vico-bad-index (e) ;cursor at start
      (declare (ignore e))
      (return-from buf:cursor-find-prev (values nil cursor)))))

;; TODO use cursor-prev-char and index tracking closure
(defmethod buf:cursor-search-next ((cursor cursor) string)
  string)

(defmethod buf:cursor-search-prev ((cursor cursor) string)
  string)

;; buffer modification

(defun data-buffer-insert (pt string)
  "Returns a pointer"
  (let ((strlen (babel:string-size-in-octets string
                                             :encoding ffi:*default-foreign-encoding*))
        (data (first (pt-data-buffers pt))))
    (if (or (null data) (< (data-buffer-capacity data) (+ strlen (data-buffer-size data))))
        (let* ((new-capacity (max strlen +min-data-buffer-size+)) ; v harmless null for now
               (new-char* (ffi:foreign-string-alloc string))
               (new-data (make-data-buffer :size strlen
                                           :capacity new-capacity
                                           :data new-char*
                                           :type :alloc))
               (lfs (push-lfs-to-vector (data-buffer-data new-data) strlen 0
                                        (data-buffer-lf-vec new-data))))
          (push new-data (pt-data-buffers pt))
          (values new-char* strlen 0 lfs (data-buffer-lf-vec new-data)))
        (let ((new-char* (ffi:lisp-string-to-foreign string data (1+ strlen)
                                                     :offset (data-buffer-size data)))
              (lf-offset (fill-pointer (data-buffer-lf-vec data)))
              (lfs (push-lfs-to-vector (data-buffer-data data)
                                       strlen
                                       (data-buffer-size data)
                                       (data-buffer-lf-vec data))))
          (incf (data-buffer-size data) strlen)
          (values new-char* strlen lf-offset lfs (data-buffer-lf-vec data))))))

;; boundary case
;;        v <- cursor
;; [prev]-[cursor-piece]
;;              v <- cursor, unchanged
;; [prev]-[new]-[cursor-piece]

;; middle of piece
;;                                v <- cursor
;; [prev]-[                cursor-piece                ]
;;                                                   v <- cursor
;; [prev]-[new-cursor-piece-left]-[new-cursor-piece]-[new-cursor-piece-right]

(defmethod buf:insert-at ((cursor cursor) string)
  (unless (zerop (length string))
    (let* ((pt (cursor-piece-table cursor))
           (piece (cursor-piece cursor))
           (prev (piece-prev (cursor-piece cursor))))
      (with-pt-lock (pt)
        (unwind-protect
             (progn
               (loop :for cursor :in (pt-tracked pt)
                     :do (bt:acquire-lock (cursor-lock cursor)))
               (multiple-value-bind (ptr strlen lf-offset lfs lf-vec)
                   (data-buffer-insert pt string)
                 (cond ((zerop (cursor-byte-offset cursor)) ;boundary case
                        ;; TODO optimize appending using END-CACHE
                        (let ((new-piece (make-piece :prev prev :next piece
                                                     :data ptr
                                                     :size strlen
                                                     :lf-offset lf-offset
                                                     :lfs lfs
                                                     :lf-vec lf-vec)))
                          (setf (piece-next prev) new-piece
                                (piece-prev piece) new-piece)))
                       ((< (cursor-byte-offset cursor) (piece-size piece))
                        (let ((new-piece (make-piece :prev prev :next piece
                                                     :data ptr
                                                     :size strlen
                                                     :lf-offset lf-offset
                                                     :lfs lfs
                                                     :lf-vec lf-vec)))))
                       (t ; off-end
                        (let* ((sentinel (piece-next piece))
                               (new-piece (make-piece :prev piece :next sentinel
                                                      :data ptr
                                                      :size strlen
                                                      :lf-offset lf-offset
                                                      :lfs lfs
                                                      :lf-vec lf-vec)))
                          (setf (piece-next piece) new-piece
                                (piece-prev sentinel) new-piece))))))
          (loop :for cursor :in (pt-tracked pt)
                :do (incf (cursor-revision cursor))
                    (bt:release-lock (cursor-lock cursor)))) ;unwind-protect ends
        (incf (pt-revision pt)))
      pt)))

(defmethod buf:erase-at ((cursor cursor) &optional count)
  (let ((pt (cursor-piece-table cursor)))
    (with-pt-lock (pt)
      count
      (incf (pt-revision pt)))))

(defparameter cursor (buf:make-cursor *pt* 0))
