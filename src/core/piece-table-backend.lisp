;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; preliminary piece table implementation
;;
;; thanks to:
;;
;; *Jürgen Böhms Heimatseiten* for his red-black tree implementation available at
;; (http://www.aviduratas.de/lisp/progs/rb-trees-struct-1-01.lisp) used in a prototype
;; ---
;; *Abiword* (lightweight word processor https://www.abisource.com/) for inspiration
;;
;; DONE inline pieces into node struct to reduce indirection
;; DONE avl-trees instead of red-black trees
;; DONE store text in (utf-8) octets
;; TODO immutable implementation for cheap clone operation when concurrent
;; - partial persistence
;; TODO buffer ownership for write access - only owner can pass ownership to another thread
;; - implemented as higher level layer mixin on BUFFER that default buffers all inherit
;; - not sensible for concurrent plugins writing to same buffer (racing with user)
;; - concurrent reads (writes are meaningless and should fail) only on clones - no locking
;;   - clone-p field in piece-table check after thread
;; TODO cleanup, write tests and split into separate library
;;

(defpackage :vico-core.buffer.piece-table
  (:use :cl :alexandria)
  (:import-from :vico-core.buffer :*max-buffer-size*)
  (:local-nicknames (:buffer :vico-core.buffer))
  (:export #:piece-table-buffer))
(in-package :vico-core.buffer.piece-table)

(defun required-arg (arg)
  (error "struct arg ~A is required" arg))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *max-optimize-settings*
    '(optimize (speed 1) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
  (deftype idx () '(integer 0 #.*max-buffer-size*))

;;; binary-tree node

  (defstruct (node (:conc-name nil)
                   (:constructor %make-node)
                   (:print-object %print-node))
    "NODE is the building block of the AVL tree that holds descriptors that constitute a document.
PIECE-BUFFER indicates which text buffer the piece corresponds to.
PIECE-OFFSET represents the offset in bytes of the start of the text referred to by the piece (into the piece's corresponding BUFFER).
PIECE-SIZE tracks the length of the text referred to by the piece.
PIECE-LF-COUNT tracks the number of linefeed characters in the text referred to by the piece.
COLOR - color parity of the node. New nodes are red by default.
LTREE-SIZE - tracks the total size of all pieces in the node's left subtree. Used to guarantee O(log n) (n being number of nodes) searches for a given offset.
LTREE-LFS - similar to LTREE-SIZE above but with linefeed characters."
    parent ; node pointers
    left
    right
    (balance-factor 0 :type (integer -2 2))
    (piece-buffer :change-buffer :type (member :change-buffer :initial-buffer))
    (piece-offset 0 :type idx)
    (piece-chars 0 :type idx)
    (piece-lf-count 0 :type idx)
    (ltree-chars 0 :type idx)
    (ltree-lfs 0 :type idx))

  (defmethod make-load-form ((obj node) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots obj)))

(define-constant +sentinel+ (%make-node :piece-chars #.*max-buffer-size*
                                        :piece-lf-count #.*max-buffer-size*)
  :test (constantly t)
  :documentation "Sentinel node - represents the empty leaves of the tree - simplifies
deletion.")
(setf (parent +sentinel+) +sentinel+
      (left +sentinel+) +sentinel+
      (right +sentinel+) +sentinel+)

(declaim (inline make-node))
(defun make-node (&key (piece-buffer :change-buffer) (balance-factor 0)
                    (piece-offset 0) (piece-chars 0) (piece-lf-count 0)
                    (ltree-chars 0) (ltree-lfs 0))
  (%make-node :left +sentinel+ :right +sentinel+ :parent +sentinel+
              :balance-factor balance-factor
              :piece-buffer piece-buffer
              :piece-offset piece-offset
              :piece-chars piece-chars
              :piece-lf-count piece-lf-count
              :ltree-chars ltree-chars :ltree-lfs ltree-lfs))

;;; tree utilities

(declaim (inline node-null
                 right-child-p left-child-p
                 grandparent
                 leftmost rightmost
                 prev-node next-node))

(defun node-null (x)
  "NULL but with nodes"
  (eq +sentinel+ (the node x)))

(defun right-child-p (node)
  (eq node (right (parent node))))

(defun left-child-p (node)
  (eq node (left (parent node))))

(defun grandparent (node)
  (parent (parent node)))

;; node printing function - needs redp

(defun %print-node (node s)
  (format s "#S(node :bf ~d :buffer ~A :offset ~d :chars ~d :lfs ~d :lchars ~d :llfs ~d)"
          (balance-factor node)
          (piece-buffer node)
          (piece-offset node)
          (piece-chars node)
          (piece-lf-count node)
          (ltree-chars node)
          (ltree-lfs node)))

(defun leftmost (x)
  (loop :until (node-null (left x))
        :do (setf x (left x))
        :finally (return x)))

(defun rightmost (x)
  (loop :until (node-null (right x))
        :do (setf x (right x))
        :finally (return x)))

(defun prev-node (x)
  (if (not (node-null (left x)))
      (rightmost (left x))
      (loop :until (node-null (parent x))
            :do (if (right-child-p x)
                    (return-from prev-node (parent x))
                    (setf x (parent x))))))

(defun next-node (x)
  (if (not (node-null (right x)))
      (leftmost (right x))
      (loop :until (node-null (parent x))
            :do (if (left-child-p x)
                    (return-from next-node (parent x))
                    (setf x (parent x))))))

(declaim (ftype (function (node) (values idx &rest nil)) calculate-size calculate-lfs))

(defun calculate-size (x)
  (declare #.*max-optimize-settings*)
  (labels ((recur (x res)
             (declare (type idx res))
             (if (node-null x)
                 res
                 (recur (right x)
                        (+ res (piece-chars x) (ltree-chars x))))))
    (recur x 0)))

(defun calculate-lfs (x)
  (declare #.*max-optimize-settings*)
  (labels ((recur (x res)
             (declare (type idx res))
             (if (node-null x)
                 res
                 (recur (right x)
                        (+ res (piece-lf-count x) (ltree-lfs x))))))
    (recur x 0)))

(defun rotate-left (node)
  (declare #.*max-optimize-settings*)
  (let ((p (parent node))
        (child (right node)))
    (incf (ltree-chars child) (+ (piece-chars node) (ltree-chars node)))
    (incf (ltree-lfs child) (+ (piece-lf-count node) (ltree-lfs node)))
    (setf (right node) (left child))
    (unless (node-null (left child))
      (setf (parent (left child)) node))
    (setf (left child) node
          (parent node) child)
    (setf (parent child) p)
    (unless (node-null p)
      (if (eq (right p) node)
          (setf (right p) child)
          (setf (left p) child)))
    (if (zerop (balance-factor child))
        (setf (balance-factor child) -1
              (balance-factor node) +1)
        (setf (balance-factor child) 0
              (balance-factor node) 0))
    child))

(defun rotate-right (node)
  (declare #.*max-optimize-settings*)
  (let ((p (parent node))
        (child (left node)))
    (decf (ltree-chars node) (+ (piece-chars child) (ltree-chars child)))
    (decf (ltree-lfs node) (+ (piece-lf-count child) (ltree-lfs child)))
    (setf (left node) (right child))
    (unless (node-null (right child))
      (setf (parent (right child)) node))
    (setf (right child) node
          (parent node) child)
    (setf (parent child) p)
    (unless (node-null p)
      (if (eq (left p) node)
          (setf (left p) child)
          (setf (right p) child)))
    (if (zerop (balance-factor child))
        (setf (balance-factor child) 1
              (balance-factor node) -1)
        (setf (balance-factor child) 0
              (balance-factor node) 0))
    child))

(declaim (inline rotate-left/right rotate-right/left))

(defun rotate-left/right (node)
  ;;(declare #.*max-optimize-settings*)
  (let* ((z (left node))
         (new-root (right z)) ; will be rotated to the root of this subtree
         (new-root-balance (balance-factor new-root)))
    (rotate-left z)
    (rotate-right node)
    (case new-root-balance
      (-1
       (setf (balance-factor node) 1
             (balance-factor z) 0))
      (0
       (setf (balance-factor node) 0
             (balance-factor z) 0))
      (1
       (setf (balance-factor node) 0
             (balance-factor z) -1)))
    (setf (balance-factor new-root) 0)
    new-root))

(defun rotate-right/left (node)
  ;;(declare #.*max-optimize-settings*)
  (let* ((z (right node))
         (new-root (left z)) ; will be rotated to the root of this subtree
         (new-root-balance (balance-factor new-root)))
    (rotate-right z)
    (rotate-left node)
    (case new-root-balance
      (-1
       (setf (balance-factor node) 0
             (balance-factor z) +1))
      (0
       (setf (balance-factor node) 0
             (balance-factor z) 0))
      (1
       (setf (balance-factor node) -1
             (balance-factor z) 0)))
    (setf (balance-factor new-root) 0)
    new-root))

(defun fix-ltree-data (x root)
  "Fix the left-subtree metadata of x's parents."
  (declare #.*max-optimize-settings*)
  (unless (eq x root)
    ;; traverse up to the first node containing x in its left subtree to get delta
    (loop :until (or (eq x root) (not (right-child-p x)))
          :do (setf x (parent x))
          :finally (when (eq x root)
                     (return-from fix-ltree-data))
                   (setf x (parent x)))
    ;; Go up towards root, propagating the change to x's parents
    ;; note: we operate on (parent root) to ensure that LEFT-CHILD-P is always
    ;; valid while also treating ROOT properly
    (loop :with char-delta = (- (calculate-size (left x)) (ltree-chars x))
          :with lf-delta = (- (calculate-lfs (left x)) (ltree-lfs x))
          :initially (incf (ltree-chars x) char-delta)
                     (incf (ltree-lfs x) lf-delta)
          :until (eq x root)
          :do (when (left-child-p x)
                (incf (ltree-chars (parent x)) char-delta)
                (incf (ltree-lfs (parent x)) lf-delta))
              (setf x (parent x)))
    (values)))

(defun fix-insert (new root)
  (declare #.*max-optimize-settings*)
  (loop :with child = new
        :for node = (parent child)
        :until (node-null node)
        :do (if (eq child (left node))
                (ecase (decf (balance-factor node))
                  (0 (return root))
                  (-1 (setf child node))
                  (-2 ;   V  V parent of new-root's subtree after rotation
                   (let ((new-root (if (= (balance-factor child) +1)
                                       (rotate-left/right node)
                                       (rotate-right node))))
                     (if (node-null (parent new-root))
                         (return new-root)
                         (return root)))))
                ;; symmetric case for child = (right node)
                (ecase (incf (balance-factor node))
                  (0 (return root))
                  (+1 (setf child node))
                  (+2
                   (let ((new-root (if (= (balance-factor child) -1)
                                       (rotate-right/left node)
                                       (rotate-left node))))
                     (if (node-null (parent new-root))
                         (return new-root)
                         (return root))))))
        :finally (return root)))

(defun fix-delete (new-root direction root)
  (declare #.*max-optimize-settings*)
  (loop :for first-time = t :then nil
        :with child = new-root
        :for node = (parent child)
        :until (node-null node)
        :do (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (eq child (left node)))
                (ecase (incf (balance-factor node))
                  (0 (setf child node))
                  (1 (return root))
                  (2
                   (let ((node-parent (parent node))
                         (right-child (right node)))
                     (if (= (balance-factor right-child) -1)
                         (setf child (rotate-right/left node))
                         (setf child (rotate-left node)))
                     (setf (parent child) node-parent)
                     (cond ((node-null node-parent)
                            (return child))
                           ((= (balance-factor right-child) -1)
                            (return root))))))
                ;; symmetric case for child = (right node)
                (ecase (decf (balance-factor node))
                  (0 (setf child node))
                  (-1 (return root))
                  (-2
                   (let ((node-parent (parent node))
                         (left-child (left node)))
                     (if (= (balance-factor left-child) +1)
                         (setf child (rotate-left/right node))
                         (setf child (rotate-right node)))
                     (setf (parent child) node-parent)
                     (cond ((node-null node-parent)
                            (return child))
                           ((= (balance-factor left-child) +1)
                            (return root)))))))
        :finally (return root)))

;;; tree algorithms

(defun insert-before (node x root)
  "Inserts NODE directly before X in the tree rooted at ROOT.
Returns the new tree root and the inserted node as multiple values."
  (declare #.*max-optimize-settings*)
  (cond ((node-null (left x))
         (setf (left x) node)
         (setf (parent node) x)
         (parent node))
        (t
         (setf x (rightmost (left x)))
         (setf (right x) node)
         (setf (parent node) x)))
  (fix-ltree-data node root)
  (values (fix-insert node root) node))

(defun insert-after (node x root)
  "Inserts NODE directly after X in the tree rooted at ROOT.
Returns the new tree root and the inserted node as multiple values."
  (declare #.*max-optimize-settings*)
  (cond ((node-null (right x))
         (setf (right x) node)
         (setf (parent node) x))
        (t
         (setf x (leftmost (right x)))
         (setf (left x) node)
         (setf (parent node) x)))
  (fix-ltree-data node root)
  (values (fix-insert node root) node))

(defun delete-node (node root)
  "Delete NODE from the tree rooted at ROOT.
Informative comments adapted from original source."
  (declare #.*max-optimize-settings*)
  (let (x z) ; x is 'actually' deleted, z is x's child (may be +sentinel+)
    (setf (piece-chars node) 0)
    (fix-ltree-data node root)
    (cond ((node-null (left node))
           (setf x node)
           (setf z (right node)))
          ((node-null (right node))
           (setf x node)
           (setf z (left node)))
          (t
           (setf x (rightmost (left node)))
           (setf (piece-offset node) (piece-offset x)
                 (piece-chars node) (piece-chars x)
                 (piece-lf-count node) (piece-lf-count x)
                 (piece-buffer node) (piece-buffer x))
           (decf (ltree-chars node) (piece-chars x))
           (decf (ltree-lfs node) (piece-lf-count x))
           (setf z (left x))))
    (let ((direction (if (left-child-p x) :left :right)))
      ;; case when x is root
      (when (eq x root)
        (setf root z)
        (setf (parent z) +sentinel+)
        (return-from delete-node root))
      ;; link z where x was
      (if (left-child-p x)
          (setf (left (parent x)) z)
          (setf (right (parent x)) z))
      ;; link parent
      (setf (parent z) (parent x))
      ;; restore invariants
      (prog1 (fix-delete z direction root)
        (setf (parent +sentinel+) +sentinel+)))))

;; piece-table specific

(declaim (ftype (function (idx node) (values node idx &rest nil)) offset-to-node))
(defun offset-to-node (offset root)
  (declare #.*max-optimize-settings*)
  "Returns the node corresponding to OFFSET in ROOT's subtree. O(log n) number of nodes.
Note that the node is chosen such that its node contains the character referred to by
OFFSET. Returns +sentinel+ if not found."
  (labels ((recur (offset node node-offset)
             (declare (type idx offset node-offset))
             (cond ((< offset (ltree-chars node))
                    (recur offset (left node) node-offset))
                   ((< offset (+ (ltree-chars node) (piece-chars node)))
                    (values node (+ node-offset (ltree-chars node))))
                   (t
                    (recur
                     (- offset (+ (ltree-chars node) (piece-chars node)))
                     (right node)
                     (+ node-offset (+ (ltree-chars node) (piece-chars node))))))))
    (declare (dynamic-extent #'recur))
    (recur offset root 0)))

(declaim (ftype (function (idx node) (values node idx idx &rest nil)) lf-to-node))
(defun lf-to-node (n root)
  (declare #.*max-optimize-settings*)
  "Returns the node containing the Nth line in ROOT's subtree, the number of linefeeds
encountered up to that point and the node's absolute offset as multiple values."
  (labels ((recur (n node lf-count offset)
             (declare (type idx n lf-count offset))
             (cond ((<= n (ltree-lfs node))
                    (recur n (left node) lf-count offset))
                   ((<= n (+ (ltree-lfs node) (piece-lf-count node)))
                    (values node (+ lf-count (ltree-lfs node))
                            (+ offset (ltree-chars node))))
                   (t
                    (recur
                     (- n (+ (ltree-lfs node) (piece-lf-count node)))
                     (right node)
                     (+ lf-count (+ (ltree-lfs node) (piece-lf-count node)))
                     (+ offset (+ (ltree-chars node) (piece-chars node))))))))
    (declare (dynamic-extent #'recur))
    (recur n root 0 0)))

(defun node-to-offset (node root)
  (declare #.*max-optimize-settings*
           (type node node root))
  "Computes the absolute offset of NODE. O(log n). Only useful for debugging."
  (loop :until (eq node root)
        :with offset :of-type idx = (ltree-chars node)
        :finally (return offset)
        :do (when (right-child-p node)
              (incf offset (+ (piece-chars (parent node))
                              (ltree-chars (parent node)))))
            (setf node (parent node))))

;;; piece-table

;; TODO text buffers must be separate objects so we can implement a (read-only) cloning
;; operation. edit operations will simply return a copy using (copy-piece-table) pointing
;; to the old buffers - text does not move - invalidate on tree collapse (do on save?)

(defstruct text-buffer
  (data (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8)))
  (fill 0 :type idx))

(defun text-buffer-append (text-buffer string)
  (declare #.*max-optimize-settings*
           (type string string))
  (let* ((old-fill (text-buffer-fill text-buffer))
         (old-actual-length (length (text-buffer-data text-buffer)))
         (string-as-octets (babel:string-to-octets string))
         (string-length-in-octets (length (the (simple-array (unsigned-byte 8)) ;TODO alias?
                                               string-as-octets)))
         (new-fill (+ old-fill string-length-in-octets)))
    (when (>= new-fill #.*max-buffer-size*)
      (error "buffer exceeded maximum size ~d!" #.*max-buffer-size*))
    (when (> new-fill old-actual-length)
      (setf (text-buffer-data text-buffer)
            (adjust-array (text-buffer-data text-buffer)
                          (max new-fill (min (* old-actual-length 2)
                                             #.*max-buffer-size*)))))
    (replace (text-buffer-data text-buffer) string-as-octets :start1 old-fill)
    (incf (text-buffer-fill text-buffer) string-length-in-octets)
    string-length-in-octets))

;; TODO two text buffers means we can implement collapsing the tree more easily: resize
;; initial-buffer and copy original pieces into place while collecting change-buffer pieces
;; copy high indexes (free space) first as ordering within initial buffer is invariant
;; if the document has shrunk, then just reallocate

(defstruct (piece-table (:conc-name pt-)
                        (:constructor %make-piece-table))
  "SIZE tracks the current size of the document in characters.
LINE-COUNT tracks the number of lines in the document.
INITIAL-BUFFER is a string holding the immutable original content of the document.
CHANGE-BUFFER is a string holding any inserted text.
ROOT is the root of the AVL tree containing PIECE descriptors.
CACHE is the most recently inserted node - used for optimization of (common) sequential inserts."
  (size (required-arg 'size) :type idx)
  (line-count (required-arg 'line-count) :type idx)
  (initial-buffer (make-text-buffer) :type text-buffer)
  (change-buffer (make-text-buffer) :type text-buffer)
  (root (required-arg 'root) :type node)
  (cache +sentinel+ :type node)
  (cache2 +sentinel+ :type node)
  (cache-offset2 0 :type idx)
  (cache-size2 0 :type idx))

(defun make-piece-table (&key (initial-contents "") initial-file)
  (let* ((initial-contents-as-octets (if initial-file
                                         (vico-core.io:file-to-bytes initial-file)
                                         (babel:string-to-octets initial-contents)))
         (length (babel:vector-size-in-chars initial-contents-as-octets))
         (linefeeds (count #.(char-code #\Newline) initial-contents-as-octets))
         (root (make-node :balance-factor 0
                          :piece-buffer :initial-buffer
                          :piece-offset 0
                          :piece-chars length
                          :piece-lf-count linefeeds)))
    (%make-piece-table
     :size length
     :line-count (1+ linefeeds)
     :initial-buffer (make-text-buffer :data initial-contents-as-octets
                                       :fill (length initial-contents-as-octets))
     :root root
     :cache2 root
     :cache-size2 length)))

(declaim (inline pt-piece-buffer
                 pt-fix-ltree-data
                 pt-insert-before pt-insert-after
                 pt-delete-node
                 pt-offset-to-node pt-lf-to-node pt-node-to-offset
                 pt-first-node pt-last-node))

(declaim (ftype (function (piece-table node) text-buffer) pt-piece-buffer))
(defun pt-piece-buffer (piece-table node)
  "Returns the data of the text-buffer associated with PIECE."
  (ecase (piece-buffer node)
    (:change-buffer (pt-change-buffer piece-table))
    (:initial-buffer (pt-initial-buffer piece-table))))

(defun pt-fix-ltree-data (piece-table x)
  (fix-ltree-data x (pt-root piece-table)))

(defun pt-insert-before (piece-table node x)
  (multiple-value-bind (new-root new)
      (insert-before node x (pt-root piece-table))
    (setf (pt-root piece-table) new-root)
    new))

(defun pt-insert-after (piece-table node x)
  (multiple-value-bind (new-root new)
      (insert-after node x (pt-root piece-table))
    (setf (pt-root piece-table) new-root)
    new))

(defun pt-delete-node (piece-table node)
  (setf (pt-root piece-table) (delete-node node (pt-root piece-table)))
  node)

(declaim (ftype (function (t idx) (values node idx)) pt-offset-to-node))
(defun pt-offset-to-node (piece-table offset)
  "Returns the node containing OFFSET in PIECE-TABLE and its absolute offset as multiple
values. Note that the node is chosen to contain the character referred to by OFFSET"
  (if (< (1- (pt-cache-offset2 piece-table)) offset (+ (pt-cache-offset2 piece-table)
                                                       (pt-cache-size2 piece-table)))
      (progn ;;(print :cache-hit)
        (values (pt-cache2 piece-table) (pt-cache-offset2 piece-table)))
      (multiple-value-bind (node offset)
          (offset-to-node offset (pt-root piece-table))
        (setf (pt-cache2 piece-table) node)
        (setf (pt-cache-offset2 piece-table) offset)
        (setf (pt-cache-size2 piece-table) (piece-chars node))
        (values node offset)))) ;set cache result

(declaim (ftype (function (t idx) (values node idx idx)) pt-lf-to-node))
(defun pt-lf-to-node (piece-table linefeed-no)
  "Returns the node containing LINEFEED-NO in PIECE-TABLE, the number of linefeeds seen up
to that point and the offset of the found node."
  (lf-to-node linefeed-no (pt-root piece-table)))

(defun pt-node-to-offset (piece-table node)
  (node-to-offset node (pt-root piece-table)))

(defun pt-first-node (piece-table)
  (leftmost (pt-root piece-table)))

(defun pt-last-node (piece-table)
  (rightmost (pt-root piece-table)))

(declaim (ftype (function ((simple-array (unsigned-byte 8)) idx idx) idx)
                nth-utf8-offset))
(defun nth-utf8-offset (buffer start n)
  (loop :with idx :of-type idx = start
        :for count :of-type idx :from 1 :to n
        :do (cond ((< (aref buffer idx) #x80)
                   (incf idx))
                  ((< (aref buffer idx) #xE0)
                   (incf idx 2))
                  ((< (aref buffer idx) #xF0)
                   (incf idx 3))
                  ((< (aref buffer idx) #xF8)
                   (incf idx 4))
                  (t (error "catch invalid utf-8 during file load")))
        :finally (return idx)))

(defmacro define-node-fn (name extra return-value &body body)
  `(progn
     (declaim (ftype (function (piece-table node idx idx) (values ,return-value &rest nil))
                     ,name)
              (inline ,name))
     (defun ,name (piece-table node start-offset end-offset ,@extra)
       (let* ((buffer (text-buffer-data (pt-piece-buffer piece-table node)))
              (byte-start (nth-utf8-offset buffer
                                           (piece-offset node)
                                           start-offset))
              (byte-end (nth-utf8-offset buffer
                                         byte-start
                                         (- end-offset start-offset))))
         ,@body))))

(define-node-fn pt-get-node-string () (simple-array character)
  (babel:octets-to-string buffer :start byte-start :end byte-end))

(define-node-fn pt-count-node-linefeeds () idx
  (count #.(char-code #\Newline) buffer :start byte-start :end byte-end))

;;; interface

(define-condition piece-table-bounds-error (error)
  ((piece-table :initarg :piece-table
                :reader pt-bounds-error-piece-table
                :type piece-table)
   (bounds :initarg :bounds
           :reader pt-bounds-error-bounds
           :type (cons idx idx)))
  (:documentation "Signaled when trying to access out of bounds."))

(define-condition piece-table-bad-offset (piece-table-bounds-error)
  ((offset :initarg :bad-offset
           :reader pt-bounds-error-offset))
  (:report (lambda (condition stream)
             (format stream "offset ~d is out of bounds for ~A. Should be an integer ~
                             within [~d:~d]."
                     (pt-bounds-error-offset condition)
                     (pt-bounds-error-piece-table condition)
                     (car (pt-bounds-error-bounds condition))
                     (cdr (pt-bounds-error-bounds condition))))))

(define-condition piece-table-bad-line-number (piece-table-bounds-error)
  ((line-number :initarg :line-number
                :reader pt-bounds-error-line-number))
  (:report (lambda (condition stream)
             (format stream "line-number ~d is out of bounds for ~A. Should be an integer ~
                             within [~d:~d]."
                     (pt-bounds-error-line-number condition)
                     (pt-bounds-error-piece-table condition)
                     (car (pt-bounds-error-bounds condition))
                     (cdr (pt-bounds-error-bounds condition))))))

(defun pt-char (piece-table n) ; silly manual optimization
  (declare #.*max-optimize-settings*
           (type idx n))
  (when (or (< n 0) (>= n (pt-size piece-table)))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset n
                                   :bounds (cons 0 (1- (pt-size piece-table)))))
  (multiple-value-bind (node node-offset)
      (pt-offset-to-node piece-table n)
    (let* ((buffer (text-buffer-data (pt-piece-buffer piece-table node)))
           (byte-idx (nth-utf8-offset buffer (piece-offset node) (- n node-offset)))
           (target-char-byte-length (cond ((< (aref buffer byte-idx) #x80) 1)
                                          ((< (aref buffer byte-idx) #xE0) 2)
                                          ((< (aref buffer byte-idx) #xF0) 3)
                                          ((< (aref buffer byte-idx) #xF8) 4)
                                          (t (error "invalid utf-8"))))
           (codepoint (if (= target-char-byte-length 1)
                          (aref buffer byte-idx)
                          (logand (aref buffer byte-idx) (ecase target-char-byte-length
                                                           (2 #x1F)
                                                           (3 #x0F)
                                                           (4 #x07))))))
      (declare (type (integer 0 #x10ffff) codepoint))
      (loop :for i :from 1 :below target-char-byte-length
            :do (setf codepoint (logior (ash codepoint 6)
                                        (logand (aref buffer (+ byte-idx i)) #x3F)))
            :finally (return-from pt-char (code-char codepoint))))))

(defun pt-subseq (piece-table start &optional (end (pt-size piece-table)))
  (declare #.*max-optimize-settings*
           (type idx start end))
  (when (> end (pt-size piece-table))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset end
                                   :bounds (cons 0 (pt-size piece-table))))
  (when (> start end)
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset start
                                   :bounds (cons 0 end)))
  (when (= start end)
    (return-from pt-subseq ""))
  ;; only search for START's node first - we can check whether END refers to the same
  ;; node without searching vvvv
  (multiple-value-bind (start-node start-node-offset)
      (pt-offset-to-node piece-table start)
    ;; (hopefully) common case when start and end are spanned by 1 node
    (when (<= end (+ start-node-offset (piece-chars start-node)))
      (return-from pt-subseq
        (pt-get-node-string piece-table start-node
                            (- start start-node-offset)
                            (- end start-node-offset))))
    ;; we build up the string in piece-sized chunks by traversing between
    ;; start-node and end-node, concatenating their pieces' text to str
    (multiple-value-bind (end-node end-node-offset)
        (pt-offset-to-node piece-table (1- end)) ; we don't need end's character
      (loop
        :with subseq = (make-array (- end start) :element-type 'character)
        ;; TODO argue with slime's indentation here
        :initially (replace subseq (pt-get-node-string piece-table start-node
                                                       (- start start-node-offset)
                                                       (piece-chars start-node)))
        :with subseq-free-offset
          :of-type idx = (- (piece-chars start-node) (- start start-node-offset))
        :for node = (next-node start-node) :then (next-node node)
        :until (eq node end-node)
        :do (replace subseq
                     (pt-get-node-string piece-table node
                                         0
                                         (piece-chars node))
                     :start1 subseq-free-offset)
            (incf subseq-free-offset (piece-chars node))
        :finally (return (replace subseq
                                  (pt-get-node-string piece-table end-node
                                                      0
                                                      (- end end-node-offset))
                                  :start1 subseq-free-offset))))))

(defun pt-line-number-offset (piece-table line-number)
  (declare #.*max-optimize-settings*
           (type idx line-number))
  (let ((line-count (pt-line-count piece-table)))
    (when (or (<= line-number 0) (> line-number line-count))
      (error 'piece-table-bad-line-number :piece-table piece-table
                                          :line-number line-number
                                          :bounds (cons 1 line-count))))
  (when (= line-number 1)
    (return-from pt-line-number-offset 0))
  ;; find nth linebreak in node - linear, but very fast
  (multiple-value-bind (node lf-count offset) ; lf-count is at least 1
      (pt-lf-to-node piece-table (1- line-number))
    (loop :with buffer = (text-buffer-data (pt-piece-buffer piece-table node))
          :with byte-idx :of-type idx = (piece-offset node)
          :with piece-lf-count :of-type idx
          :for character-count :of-type idx :from 0
          :until (= (1- line-number) (+ lf-count piece-lf-count))
          :do (cond ((< (aref buffer byte-idx) #x80)
                     (when (= (aref buffer byte-idx) #.(char-code #\Newline))
                       (incf piece-lf-count))
                     (incf byte-idx))
                    ((< (aref buffer byte-idx) #xE0)
                     (incf byte-idx 2))
                    ((< (aref buffer byte-idx) #xF0)
                     (incf byte-idx 3))
                    ((< (aref buffer byte-idx) #xF8)
                     (incf byte-idx 4))
                    (t (error "catch invalid utf-8 during file load")))
          :finally (return (+ offset character-count)))))

(defun pt-offset-in-bytes (piece-table offset)
  (declare #.*max-optimize-settings*
           (type idx offset))
  (when (or (< offset 0) (> offset (pt-size piece-table)))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset offset
                                   :bounds (cons 0 (pt-size piece-table))))
  ;; TODO not fast enough especially on large files because O(n)
  ;; uncommon operation though...not sure if worth bloating node structure further
  (when (= offset 0) (return-from pt-offset-in-bytes 0))
  (multiple-value-bind (end-node end-offset)
      (pt-offset-to-node piece-table (1- offset))
    (loop
      :with bytes :of-type idx
      :for node = (pt-first-node piece-table) :then (next-node node)
      :until (eq node end-node)
      :do (incf bytes (- (nth-utf8-offset (text-buffer-data
                                           (pt-piece-buffer piece-table node))
                                          (piece-offset node) (piece-chars node))
                         (piece-offset node)))
      :finally (return
                 (+ bytes
                    (- (nth-utf8-offset (text-buffer-data
                                         (pt-piece-buffer piece-table end-node))
                                        (piece-offset end-node) (- offset end-offset))
                       (piece-offset end-node)))))))

(defun pt-byte-length (piece-table)
  (+ (text-buffer-fill (pt-initial-buffer piece-table))
     (text-buffer-fill (pt-change-buffer piece-table))))

(defun pt-insert (piece-table string offset)
  (declare #.*max-optimize-settings*
           (type string string)
           (type idx offset))
  (when (> offset (pt-size piece-table))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset offset
                                   :bounds (cons 0 (pt-size piece-table))))
  (let ((old-change-buffer-size (text-buffer-fill (pt-change-buffer piece-table)))
        (length (length string))
        (lf-count (count #\Newline string)))
    (unless (= length 0)
      (text-buffer-append (pt-change-buffer piece-table) string)
      (cond ((= (pt-size piece-table) 0)
             (let* ((new-node (make-node :piece-buffer :change-buffer
                                         :piece-offset old-change-buffer-size
                                         :piece-chars length
                                         :piece-lf-count lf-count
                                         :balance-factor 0)))
               (setf (pt-cache piece-table) new-node)
               (setf (pt-root piece-table) new-node)))
            ;; table is nonempty, insertion before start - cannot be cached
            ((= offset 0)
             (setf (pt-cache piece-table)
                   (pt-insert-before piece-table
                                     (make-node :piece-buffer :change-buffer
                                                :piece-offset old-change-buffer-size
                                                :piece-chars length
                                                :piece-lf-count lf-count)
                                     (pt-first-node piece-table))))
            (t
             (multiple-value-bind (node node-offset)
                 (pt-offset-to-node piece-table (1- offset))
               (let ((boundary (= offset (+ node-offset (piece-chars node)))))
                 ;; common case - appending to (cached) node corresponding to the
                 ;; end of the 'change' piece-table
                 (if (and (eq node (pt-cache piece-table)) boundary)
                     (progn
                       (incf (piece-chars node) length)
                       (incf (pt-cache-size2 piece-table) length)
                       (incf (piece-lf-count node) lf-count)
                       (pt-fix-ltree-data piece-table node))
                     (let ((new (make-node :piece-buffer :change-buffer
                                           :piece-offset old-change-buffer-size
                                           :piece-chars length
                                           :piece-lf-count lf-count)))
                       (if boundary ; insertion just after node boundary
                           (setf (pt-cache piece-table)
                                 (pt-insert-after piece-table new node))
                           (let* ((right-size
                                    (- (+ node-offset (piece-chars node)) offset))
                                  (right-offset (nth-utf8-offset
                                                 (text-buffer-data
                                                  (pt-piece-buffer piece-table node))
                                                 (piece-offset node)
                                                 (- offset node-offset)))
                                  (right-lfs
                                    (pt-count-node-linefeeds piece-table node
                                                             (- (piece-chars node)
                                                                right-size)
                                                             (piece-chars node)))
                                  (new-right
                                    (make-node :piece-buffer (piece-buffer node)
                                               :piece-offset right-offset
                                               :piece-chars right-size
                                               :piece-lf-count right-lfs)))
                             (decf (pt-cache-size2 piece-table)
                                   (- (+ node-offset (piece-chars node)) offset))
                             (pt-insert-after piece-table new-right node)
                             (setf (pt-cache piece-table)
                                   (pt-insert-after piece-table new node))
                             (decf (piece-chars node) right-size)
                             (decf (piece-lf-count node) right-lfs)
                             (pt-fix-ltree-data piece-table node)))))))))
      (incf (pt-size piece-table) length)
      (incf (pt-line-count piece-table) lf-count))
    (values))) ;corresponds to outermost let

(defun pt-erase-within-piece (piece-table node node-offset start end)
  (declare #.*max-optimize-settings*
           (type idx start end node-offset))
  (let* ((delta (- end start))
         (start-on-boundary (= start node-offset))
         (end-on-boundary (= end (+ node-offset (piece-chars node)))))
    (cond ((and start-on-boundary end-on-boundary)
           (let ((delete (pt-get-node-string piece-table node
                                             0
                                             (piece-chars node))))
             (decf (pt-line-count piece-table) (piece-lf-count node))
             (pt-delete-node piece-table node)
             delete))
          (start-on-boundary
           (let* ((delete (pt-get-node-string piece-table node
                                              0
                                              (- end node-offset)))
                  (lf-delta (pt-count-node-linefeeds piece-table node
                                                     (- end node-offset)
                                                     (piece-chars node))))
             (declare (type idx lf-delta))
             (decf (pt-line-count piece-table) lf-delta)
             (setf (piece-offset node) (nth-utf8-offset
                                        (text-buffer-data
                                         (pt-piece-buffer piece-table node))
                                        (piece-offset node)
                                        delta))
             (decf (piece-chars node) delta)
             (decf (piece-lf-count node) lf-delta)
             (pt-fix-ltree-data piece-table node)
             delete))
          (end-on-boundary
           (let* ((delete (pt-get-node-string piece-table node
                                              (- start node-offset)
                                              (piece-chars node)))
                  (lf-delta (pt-count-node-linefeeds piece-table node
                                                     0
                                                     (- start node-offset))))
             (declare (type idx lf-delta))
             (decf (pt-line-count piece-table) lf-delta)
             (setf (piece-chars node) (- start node-offset))
             (decf (piece-lf-count node) lf-delta)
             (pt-fix-ltree-data piece-table node)
             delete))
          (t
           (let* ((delete (pt-get-node-string piece-table node
                                              (- start node-offset)
                                              (- end node-offset)))
                  (before-lf (pt-count-node-linefeeds piece-table node
                                                      0
                                                      (- start node-offset)))
                  (after-lf (pt-count-node-linefeeds piece-table node
                                                     (- end node-offset)
                                                     (piece-chars node))))
             (declare (type idx before-lf after-lf))
             (decf (pt-line-count piece-table) (- (piece-lf-count node)
                                                  (+ before-lf after-lf)))
             (pt-insert-after piece-table
                              (make-node :piece-buffer (piece-buffer node)
                                         :piece-offset (nth-utf8-offset
                                                        (text-buffer-data
                                                         (pt-piece-buffer piece-table node))
                                                        0
                                                        (- end node-offset))
                                         :piece-chars (- (+ node-offset (piece-chars node))
                                                         end)
                                         :piece-lf-count after-lf)
                              node)
             (setf (piece-chars node) (- start node-offset))
             (setf (piece-lf-count node) before-lf)
             (pt-fix-ltree-data piece-table node)
             delete)))))

(defun pt-erase-multiple (piece-table start-node start-node-offset start end)
  (declare #.*max-optimize-settings*
           (type idx start end start-node-offset))
  (multiple-value-bind (end-node end-node-offset)
      (pt-offset-to-node piece-table (1- end))
    (setf (pt-cache-offset2 piece-table) (nth-utf8-offset
                                          (text-buffer-data
                                           (pt-piece-buffer piece-table end-node))
                                          0
                                          (- end end-node-offset)))
    (decf (pt-cache-size2 piece-table) (- end end-node-offset))
    (loop
      :with return-buffer = (make-array (- end start) :element-type 'character)
      :with ret-buffer-start1 :of-type idx = (- (- end start) (- end end-node-offset))
      :initially (replace return-buffer
                          (pt-get-node-string piece-table start-node
                                              (- start start-node-offset)
                                              (piece-chars start-node)))
                 (replace return-buffer
                          (pt-get-node-string piece-table end-node
                                              0
                                              (- end end-node-offset))
                          :start1 ret-buffer-start1)
                 (when (eq (next-node start-node) end-node)
                   (loop-finish))
      :with start-offset = (piece-offset start-node)
      :with start-buffer = (piece-buffer start-node)
      :for delete-node = (prev-node end-node)
      :until (and (= (piece-offset delete-node) start-offset)
                  (eq (piece-buffer delete-node) start-buffer))
      :do (decf ret-buffer-start1 (piece-chars delete-node))
          (replace return-buffer
                   (pt-get-node-string piece-table delete-node
                                       0
                                       (piece-chars delete-node))
                   :start1 ret-buffer-start1)
          (decf (pt-line-count piece-table) (piece-lf-count delete-node))
          (pt-delete-node piece-table delete-node)
      :finally (if (= start start-node-offset)
                   (progn
                     (decf (pt-line-count piece-table) (piece-lf-count start-node))
                     (pt-delete-node piece-table start-node))
                   (let* ((start-node (or delete-node start-node)) ;LOOP-FINISH won't set
                          (lf-delta (pt-count-node-linefeeds piece-table start-node
                                                             (- start start-node-offset)
                                                             (piece-chars start-node))))
                     (declare (type idx lf-delta))
                     (decf (pt-line-count piece-table) lf-delta)
                     (decf (piece-lf-count start-node) lf-delta)
                     (setf (piece-chars start-node) (- start start-node-offset))
                     (pt-fix-ltree-data piece-table start-node)))
               (if (= end (+ end-node-offset (piece-chars end-node)))
                   (progn
                     (decf (pt-line-count piece-table) (piece-lf-count end-node))
                     (pt-delete-node piece-table end-node))
                   (let ((lf-delta (pt-count-node-linefeeds piece-table end-node
                                                            0
                                                            (- end end-node-offset))))
                     (declare (type idx lf-delta))
                     (decf (pt-line-count piece-table) lf-delta)
                     (decf (piece-lf-count end-node) lf-delta)
                     (decf (piece-chars end-node) (- end end-node-offset))
                     (setf (piece-offset end-node) (nth-utf8-offset
                                                    (text-buffer-data
                                                     (pt-piece-buffer piece-table end-node))
                                                    0
                                                    (- end end-node-offset)))
                     (pt-fix-ltree-data piece-table end-node)))
               (return return-buffer))))

(defun pt-erase (piece-table start &optional (end (1+ start)))
  (declare #.*max-optimize-settings*
           (type idx start end))
  (when (< end start)
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset start
                                   :bounds (cons 0 end)))
  (when (> end (pt-size piece-table))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :bad-offset end
                                   :bounds (cons 0 (pt-size piece-table))))
  (decf (pt-size piece-table) (- end start))
  (if (= start end)
      ""
      (multiple-value-bind (start-node start-node-offset)
          (pt-offset-to-node piece-table start)
        ;; if size becomes 0, cache is invalid and unreachable - we don't mind
        (decf (pt-cache-size2 piece-table)
              (- (+ (piece-chars start-node) start-node-offset) start))
        ;; (hopefully) common case when start and end are spanned by 1 node
        (if (<= end (+ start-node-offset (piece-chars start-node)))
            (pt-erase-within-piece piece-table start-node start-node-offset start end)
            (pt-erase-multiple piece-table start-node start-node-offset start end)))))

;;; Debugging & testing

(defun tree-to-list (root)
  "Returns the contents of the binary tree rooted at ROOT as a list."
  (cond ((node-null root) nil)
        (t
         (list root
               (tree-to-list (left root))
               (tree-to-list (right root))))))

(defun pt-tree (piece-table)
  (tree-to-list (pt-root piece-table)))

(defun pt-contents (piece-table)
  (pt-subseq piece-table 0))

(defun check-invariants (root)
  (labels ((recur (node)
             (if (node-null node)
                 0
                 (let ((left-height (recur (left node)))
                       (right-height (recur (right node))))
                   (when (and left-height right-height
                              (= (+ left-height (balance-factor node)) right-height))
                     (1+ (max left-height right-height)))))))
    (recur root)))

(defun check-ltree (root)
  "Checks that the metadata stored in nodes is consistent in the tree with root ROOT.
Returns t upon success, nil otherwise.
TODO put in piece-table tests"
  (loop :for x = (leftmost root) :then (next-node x)
        :until (null x)
        :do (unless (and (= (calculate-size (left x)) (ltree-chars x)))
              (format t "inconsistent node: size: ~d ltree: ~d~&"
                      (calculate-size (left x)) (ltree-chars x))
              (return-from check-ltree (values nil x)))
            (unless (and (= (calculate-lfs (left x)) (ltree-lfs x)))
              (format t "inconsistent node: lfs: ~d ltree: ~d~&"
                      (calculate-lfs (left x)) (ltree-lfs x))
              (return-from check-ltree (values nil x))))
  t)

(defun tree-test (root n)
  (declare #.*max-optimize-settings*
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((nodes 1))
    (print :insert)
    (loop :repeat n
          :for node = (leftmost root)
          :do (dotimes (i (random nodes))
                (setf node (next-node node)))
              (setf root (insert-after (make-node :piece-chars nodes) node root))
              (assert (check-ltree root))
              (assert (check-invariants root))
              (incf nodes))
    (print :delete)
    (loop :repeat n
          :for node = (leftmost root)
          :do (dotimes (i (random nodes))
                (setf node (next-node node)))
              (setf root (delete-node node root))
              (assert (check-ltree root))
              (assert (check-invariants root))
              (decf nodes))
    root))

(defun pt-test (piece-table n length)
  "Insert N random strings of LENGTH into PIECE-TABLE."
  (trivial-garbage:gc :full t)
  (let (words)
    (format t "generating words...~%")
    (dotimes (i n)
      (let ((str (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
        (dotimes (j length)
          (vector-push-extend ;(code-char (+ 161 (random 1000))) str
           (code-char (+ 33 (random 94))) str))
        (push str words)))
    (format t "inserting words...~%")
    (time
     (dotimes (i n)
       (pt-insert piece-table "this" (random (pt-size piece-table)))))))

(defun node-count (piece-table)
  (loop :for counter from 1
        :for node = (leftmost (pt-root piece-table))
          :then (next-node node)
        :while node
        :finally (return counter)))

;; ;; delete-node tests

;; ;; cond branch 1

;; ;;                               +2+
;; ;;                              /-  \-
;; ;;                            /-      \-
;; ;;                          /-          \-
;; ;;                        /-              \-
;; ;;                      /-                  \-
;; ;;                    /-                      \-
;; ;;                  /-                          \-
;; ;;                +1+   <-- to be deleted (x,node)  +3+
;; ;;               /-  \-
;; ;;             /-      \-
;; ;;           /-          \-
;; ;;         /-              \-
;; ;;       /-                  \-
;; ;;     /-                      \-
;; ;; +sentinel+               +sentinel+ -> z

;; (let* ((+1+ (%make-node :left +sentinel+
;;                         :right +sentinel+
;;                         :color :red
;;                         :piece-chars 1
;;                         :ltree-size 0))
;;        (+2+ (%make-node :left +1+
;;                         :color :black
;;                         :piece-chars 2
;;                         :ltree-size 1))
;;        (+3+ (%make-node :parent +2+
;;                         :left +sentinel+
;;                         :right +sentinel+
;;                         :color :red
;;                         :piece-chars 3
;;                         :ltree-size 0)))
;;   (setf (parent +1+) +2+)
;;   (setf (right +2+) +3+)
;;   (delete-node +1+ +2+)
;;   (assert (check-rbt +2+))
;;   (assert (check-ltree +2+)))

;; ;; cond branch 2

;; ;;TODO

;; (let* ()
;;   )

;; ;; cond branch t

;; ;;                              +5+
;; ;;                             /-  \-
;; ;;                           /-      \-
;; ;;                         /-          \-
;; ;;                       /-              \-
;; ;;                     /-                  \-
;; ;;                   /-                      \-
;; ;;                 /-                         +6+
;; ;;               +2+   <-- to be deleted (node)
;; ;;              /-  \-
;; ;;            /-      \-
;; ;;          /-          \-
;; ;;        /-              \-
;; ;;      /-                  \-
;; ;;    /-                      \-
;; ;;  +1+                        +4+
;; ;;                           -/
;; ;;                         -/
;; ;;                       -/
;; ;;                     -/
;; ;;                   -/
;; ;;                 -/
;; ;;               +3+ -> x, +sentinel+ -> z

;; (let* ((+1+ (%make-node :left +sentinel+
;;                         :right +sentinel+
;;                         :color :black
;;                         :piece-chars 1
;;                         :ltree-size 0))
;;        (+2+ (%make-node :left +1+
;;                         :right nil
;;                         :color :red
;;                         :piece-chars 2
;;                         :ltree-size 1))
;;        (+3+ (%make-node :left +sentinel+
;;                         :right +sentinel+
;;                         :color :red
;;                         :piece-chars 3
;;                         :ltree-size 0))
;;        (+4+ (%make-node :left +3+
;;                         :right +sentinel+
;;                         :parent +2+
;;                         :color :black
;;                         :piece-chars 4
;;                         :ltree-size 3))
;;        (+5+ (%make-node :left +2+
;;                         :right nil
;;                         :color :black
;;                         :piece-chars 5
;;                         :ltree-size 10))
;;        (+6+ (%make-node :left +sentinel+
;;                         :right +sentinel+
;;                         :parent +5+
;;                         :color :black
;;                         :piece-chars 6
;;                         :ltree-size 0)))
;;   (setf (parent +1+) +2+)
;;   (setf (right +2+) +4+)
;;   (setf (parent +3+) +4+)
;;   (setf (parent +2+) +5+)
;;   (setf (right +5+) +6+)
;;   (delete-node +2+ +5+)
;;   (assert (check-rbt +5+))
;;   (assert (check-ltree +5+)))

;; empty insertion case

(let ((piece-table (make-piece-table)))
  (pt-insert piece-table "hi" 0)
  (assert (string= (pt-contents piece-table) "hi")))

;; (defparameter *piece-table*
;;   (make-piece-table :initial-contents (format nil "line 1~@
;;                                                    line 2~@
;;                                                    line 3~@
;;                                                    line 4")))
;; (assert (string= (pt-contents *piece-table*) (format nil "line 1~@
;;                                                           line 2~@
;;                                                           line 3~@
;;                                                           line 4")))

;; (pt-insert *piece-table* "bàààààb" 5)
;; (assert (string= (pt-contents *piece-table*) (format nil "line bàààààb1~@
;;                                                           line 2~@
;;                                                           line 3~@
;;                                                           line 4")))
;; (assert (= (pt-byte-length *piece-table*) 39))
;; (assert (= (pt-offset-in-bytes *piece-table* (pt-size *piece-table*)) 39))

#+sbcl (require 'sb-sprof)

;;; class

(defclass piece-table-buffer (buffer:buffer)
  ((%piece-table-struct :type piece-table)))

(defmethod initialize-instance :after ((buffer piece-table-buffer) &key initial-contents
                                                                     initial-file)
  (setf (slot-value buffer '%piece-table-struct)
        (apply #'make-piece-table (if initial-file
                                      (list :initial-file initial-file)
                                      (when initial-contents
                                        (list :initial-contents initial-contents))))))

(defmethod buffer:length ((buffer piece-table-buffer))
  (pt-size (slot-value buffer '%piece-table-struct)))

(defmethod buffer:line-count ((buffer piece-table-buffer))
  (pt-line-count (slot-value buffer '%piece-table-struct)))

(defmethod buffer:char ((buffer piece-table-buffer) n)
  (pt-char (slot-value buffer '%piece-table-struct) n))

(defmethod buffer:subseq ((buffer piece-table-buffer) start &optional end)
  (apply #'pt-subseq (slot-value buffer '%piece-table-struct)
         start (when end (list end))))

(defmethod buffer:line-number-offset ((buffer piece-table-buffer) line-number)
  (pt-line-number-offset (slot-value buffer '%piece-table-struct) line-number))

(defmethod buffer:offset-in-bytes ((buffer piece-table-buffer) offset)
  (pt-offset-in-bytes (slot-value buffer '%piece-table-struct) offset))

(defmethod buffer:insert ((buffer piece-table-buffer) string offset)
  (pt-insert (slot-value buffer '%piece-table-struct) string offset))

(defmethod buffer:erase ((buffer piece-table-buffer) start &optional end)
  (apply #'pt-insert (slot-value buffer '%piece-table-struct)
         start (when end (list end))))
