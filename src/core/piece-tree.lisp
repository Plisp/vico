;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; piece table implementation - unused, kept for sentimental value
;;
;; thanks to:
;;
;; *Jürgen Böhms Heimatseiten* for his red-black tree implementation available at
;; (http://www.aviduratas.de/lisp/progs/rb-trees-struct-1-01.lisp) used in a prototype
;; ---
;; *Abiword* (lightweight word processor https://www.abisource.com/) for inspiration
;;
;; DONE inline pieces into node struct to reduce indirection (still an issue)
;; DONE avl-trees instead of red-black trees (better tree depth)
;; DONE store text in (utf-8) octets
;; CANCELLED clean up this garbage
;; CANCELLED use mmap()
;; CANCELLED use arbitrary octet encodings with babel
;; CANCELLED cleanup, write tests and split into separate library
;; TODO rewrite
;;

(defpackage :vico-core.buffer.piece-tree
  (:use :cl :alexandria)
  (:import-from :vico-core.buffer :*max-buffer-size*)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:conditions :vico-core.conditions))
  (:export #:piece-table-buffer #:*lock-piece-table*))
(in-package :vico-core.buffer.piece-tree)

(defun required-arg (arg)
  (error "struct arg ~A is required" arg))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *max-optimize-settings*
    '(optimize (speed 3) (safety 1) (debug 0) (space 0) (compilation-speed 0)))
  (deftype idx () '(integer 0 #.*max-buffer-size*))

;;; binary-tree node

  (defstruct (node (:conc-name nil)
                   (:constructor %make-node)
                   (:print-object %print-node))
    "NODE is the building block of the AVL tree that holds descriptors that constitute a document.
PIECE-BUFFER indicates which text buffer the piece corresponds to.
PIECE-OFFSET represents the offset in bytes of the start of the text referred to by the piece (into the piece's corresponding BUFFER).
PIECE-SIZE tracks the length of the text referred to by the piece in characters.
PIECE-LF-COUNT tracks the number of linefeed characters in the text referred to by the piece.
COLOR - color parity of the node. New nodes are red by default.
LTREE-SIZE - tracks the total size of all pieces in the node's left subtree. Used to guarantee O(log n) (n being number of nodes) searches for a given index.
LTREE-LFS - similar to LTREE-SIZE above but with linefeed characters."
    parent ; node pointers
    left
    right
    (balance-factor 0 :type (integer -2 2))
    (piece-buffer :change-buffer :type (member :change-buffer :initial-buffer))
    (piece-offset 0 :type idx)
    (piece-chars 1 :type idx)
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

(declaim (inline node-null right-child-p left-child-p grandparent leftmost rightmost))

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
  (let ((parent (parent node))
        (child (right node)))
    (incf (ltree-chars child) (+ (piece-chars node) (ltree-chars node)))
    (incf (ltree-lfs child) (+ (piece-lf-count node) (ltree-lfs node)))
    (setf (right node) (left child))
    (unless (node-null (left child))
      (setf (parent (left child)) node))
    (setf (left child) node
          (parent child) parent
          (parent node) child)
    (unless (node-null parent)
      (if (eq (right parent) node)
          (setf (right parent) child)
          (setf (left parent) child)))
    (if (zerop (balance-factor child))
        (setf (balance-factor child) -1
              (balance-factor node) +1)
        (setf (balance-factor child) 0
              (balance-factor node) 0))
    child))

(defun rotate-right (node)
  (declare #.*max-optimize-settings*)
  (let ((parent (parent node))
        (child (left node)))
    (decf (ltree-chars node) (+ (piece-chars child) (ltree-chars child)))
    (decf (ltree-lfs node) (+ (piece-lf-count child) (ltree-lfs child)))
    (setf (left node) (right child))
    (unless (node-null (right child))
      (setf (parent (right child)) node))
    (setf (right child) node
          (parent child) parent
          (parent node) child)
    (unless (node-null parent)
      (if (eq (left parent) node)
          (setf (left parent) child)
          (setf (right parent) child)))
    (if (zerop (balance-factor child))
        (setf (balance-factor child) +1
              (balance-factor node) -1)
        (setf (balance-factor child) 0
              (balance-factor node) 0))
    child))

(defun rotate-left/right (node)
  (declare #.*max-optimize-settings*)
  (let* ((z (left node))
         (new-root (right z)))
    ;; (rotate-left z)
    (incf (ltree-chars new-root) (+ (piece-chars z) (ltree-chars z)))
    (incf (ltree-lfs new-root) (+ (piece-lf-count z) (ltree-lfs z)))
    (setf (right z) (left new-root))
    (unless (node-null (left new-root))
      (setf (parent (left new-root)) z))
    (setf (left new-root) z
          (parent new-root) node
          (parent z) new-root)
    (unless (node-null node)
      (if (eq (right node) z)
          (setf (right node) new-root)
          (setf (left node) new-root)))
    ;; (rotate-right node)
    (let ((parent (parent node)))
      (decf (ltree-chars node) (+ (piece-chars new-root) (ltree-chars new-root)))
      (decf (ltree-lfs node) (+ (piece-lf-count new-root) (ltree-lfs new-root)))
      (setf (left node) (right new-root))
      (unless (node-null (right new-root))
        (setf (parent (right new-root)) node))
      (setf (right new-root) node
            (parent new-root) parent
            (parent node) new-root)
      (unless (node-null parent)
        (if (eq (left parent) node)
            (setf (left parent) new-root)
            (setf (right parent) new-root))))
    ;; fix balance factors
    (case (balance-factor new-root)
      (-1
       (setf (balance-factor node) +1
             (balance-factor z) 0))
      (0
       (setf (balance-factor node) 0
             (balance-factor z) 0))
      (+1
       (setf (balance-factor node) 0
             (balance-factor z) -1)))
    (setf (balance-factor new-root) 0)
    new-root))

(defun rotate-right/left (node)
  (declare #.*max-optimize-settings*)
  (let* ((z (right node))
         (new-root (left z))) ; will be rotated to the root of this subtree
    ;; (rotate-right z)
    (decf (ltree-chars z) (+ (piece-chars new-root) (ltree-chars new-root)))
    (decf (ltree-lfs z) (+ (piece-lf-count new-root) (ltree-lfs new-root)))
    (setf (left z) (right new-root))
    (unless (node-null (right new-root))
      (setf (parent (right new-root)) z))
    (setf (right new-root) z
          (parent new-root) node
          (parent z) new-root)
    (unless (node-null node)
      (if (eq (left node) z)
          (setf (left node) new-root)
          (setf (right node) new-root)))
    ;; (rotate-left node)
    (let ((parent (parent node)))
      (incf (ltree-chars new-root) (+ (piece-chars node) (ltree-chars node)))
      (incf (ltree-lfs new-root) (+ (piece-lf-count node) (ltree-lfs node)))
      (setf (right node) (left new-root))
      (unless (node-null (left new-root))
        (setf (parent (left new-root)) node))
      (setf (left new-root) node
            (parent new-root) parent
            (parent node) new-root)
      (unless (node-null parent)
        (if (eq (right parent) node)
            (setf (right parent) new-root)
            (setf (left parent) new-root))))
    ;; fix balance factors
    (case (balance-factor new-root)
      (-1
       (setf (balance-factor node) 0
             (balance-factor z) +1))
      (0
       (setf (balance-factor node) 0
             (balance-factor z) 0))
      (+1
       (setf (balance-factor node) -1
             (balance-factor z) 0)))
    (setf (balance-factor new-root) 0)
    new-root))

(defun fix-ltree-data (x root dchars dlfs)
  "Fix the left-subtree metadata of x's parents."
  `(declare #.*max-optimize-settings*
            (type (integer ,(- #.*max-buffer-size*) #.*max-buffer-size*) dchars dlfs))
  (unless (eq x root)
    (loop :for parent = x :then (parent parent)
          :until (eq parent root)
          :do (when (left-child-p parent)
                (incf (ltree-chars (parent parent)) dchars)
                (incf (ltree-lfs (parent parent)) dlfs))
          :finally (return t))))

(defun fix-insert (new)
  (declare #.*max-optimize-settings*)
  (loop :with child = new
        :for node = (parent child)
        :until (node-null node)
        :do (if (eq child (left node))
                (ecase (decf (balance-factor node))
                  (0 (return))
                  (-1 (setf child node))
                  (-2 ;   V  V parent of new-root's subtree after rotation
                   (let ((new-root (if (= (balance-factor child) +1)
                                       (rotate-left/right node)
                                       (rotate-right node))))
                     (if (node-null (parent new-root))
                         (return new-root)
                         (return)))))
                ;; symmetric case for child = (right node)
                (ecase (incf (balance-factor node))
                  (0 (return))
                  (+1 (setf child node))
                  (+2
                   (let ((new-root (if (= (balance-factor child) -1)
                                       (rotate-right/left node)
                                       (rotate-left node))))
                     (if (node-null (parent new-root))
                         (return new-root)
                         (return))))))))

(defun fix-delete (new-root parent-was-left-child)
  (declare #.*max-optimize-settings*)
  (loop :with child = new-root
        :for node = (parent child)
        :until (node-null node)
        :do (if (or (and (node-null child) parent-was-left-child)
                    (and (not (node-null child)) (eq child (left node))))
                (ecase (incf (balance-factor node))
                  (0 (setf child node))
                  (+1 (return))
                  (+2
                   (let ((right-child (right node)))
                     (if (= (balance-factor right-child) -1)
                         (setf child (rotate-right/left node))
                         (setf child (rotate-left node)))
                     (cond ((node-null (parent child))
                            (return child))
                           ((= (balance-factor right-child) -1)
                            (return))))))
                ;; symmetric case for child = (right node)
                (ecase (decf (balance-factor node))
                  (0 (setf child node))
                  (-1 (return))
                  (-2
                   (let ((left-child (left node)))
                     (if (= (balance-factor left-child) +1)
                         (setf child (rotate-left/right node))
                         (setf child (rotate-right node)))
                     (cond ((node-null (parent child))
                            (return child))
                           ((= (balance-factor left-child) +1)
                            (return)))))))))

;;; tree algorithms

(defun insert-before (node x root)
  "Inserts NODE directly before X in the tree rooted at ROOT.
Returns the new tree root or NIL."
  (declare #.*max-optimize-settings*)
  (cond ((node-null (left x))
         (setf (left x) node)
         (setf (parent node) x)
         (parent node))
        (t
         (setf x (rightmost (left x)))
         (setf (right x) node)
         (setf (parent node) x)))
  (fix-ltree-data node root (piece-chars node) (piece-lf-count node))
  (fix-insert node))

(defun insert-after (node x root)
  "Inserts NODE directly after X in the tree rooted at ROOT.
Returns the new tree root or NIL."
  (declare #.*max-optimize-settings*)
  (cond ((node-null (right x))
         (setf (right x) node)
         (setf (parent node) x))
        (t
         (setf x (leftmost (right x)))
         (setf (left x) node)
         (setf (parent node) x)))
  (fix-ltree-data node root (piece-chars node) (piece-lf-count node))
  (fix-insert node))

(defun delete-node (node root)
  "Delete NODE from the tree rooted at ROOT."
  (declare #.*max-optimize-settings*)
  (let (x z) ; x is 'actually' deleted, z is x's child (may be +sentinel+)
    (fix-ltree-data node root (- (piece-chars node)) (- (piece-lf-count node)))
    (cond ((node-null (left node))
           (setf x node)
           (setf z (right node)))
          ((node-null (right node))
           (setf x node)
           (setf z (left node)))
          (t
           (setf x (rightmost (left node)))
           (let ((old-index (node-to-index x root)))
             (setf (piece-offset node) (piece-offset x)
                   (piece-chars node) (piece-chars x)
                   (piece-lf-count node) (piece-lf-count x)
                   (piece-buffer node) (piece-buffer x))
             (decf (ltree-chars node) (piece-chars x))
             (decf (ltree-lfs node) (piece-lf-count x))
             (setf (piece-chars x) 0
                   (piece-offset x) old-index)
             (setf z (left x)))))
    ;; case when x is root
    (when (eq x root)
      (setf (parent z) +sentinel+)
      (return-from delete-node z))
    (let ((x-was-left-child (left-child-p x)))
      ;; link z where x was
      (if x-was-left-child
          (setf (left (parent x)) z)
          (setf (right (parent x)) z))
      ;; link parent
      (setf (parent z) (parent x))
      ;; restore invariants
      (fix-delete z x-was-left-child))))

;; piece-table specific

(declaim (ftype (function (idx node) (values node idx &rest nil)) index-to-node))
(defun index-to-node (index root)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  "Returns the node corresponding to INDEX in ROOT's subtree. O(log n) number of nodes.
Note that the node is chosen such that its node contains the character referred to by
INDEX. Returns +sentinel+ if not found."
  (labels ((recur (index node node-index)
             (declare (type idx index node-index))
             (cond ((< index (ltree-chars node))
                    (recur index (left node) node-index))
                   ((< index (+ (ltree-chars node) (piece-chars node)))
                    (values node (+ node-index (ltree-chars node))))
                   (t
                    (recur
                     (- index (+ (ltree-chars node) (piece-chars node)))
                     (right node)
                     (+ node-index (+ (ltree-chars node) (piece-chars node))))))))
    (declare (dynamic-extent #'recur))
    (recur index root 0)))

(declaim (ftype (function (t node) idx) node-to-index))
(defun node-to-index (node root)
  "Computes the absolute index of NODE. O(log n). Only useful for debugging."
  (declare #.*max-optimize-settings*)
  (loop :until (eq node root)
        :with index :of-type idx = (ltree-chars node)
        :finally (return index)
        :do (when (right-child-p node)
              (incf index (+ (piece-chars (parent node))
                             (ltree-chars (parent node)))))
            (setf node (parent node))))

(declaim (ftype (function (idx node) (values node idx idx &rest nil)) lf-to-node))
(defun lf-to-node (n root)
  (declare #.*max-optimize-settings*)
  "Returns the node containing the Nth line in ROOT's subtree, the number of linefeeds
encountered up to that point and the node's absolute index as multiple values."
  (labels ((recur (n node lf-offset index)
             (declare (type idx n lf-offset index))
             (cond ((<= n (ltree-lfs node))
                    (recur n (left node) lf-offset index))
                   ((<= n (+ (ltree-lfs node) (piece-lf-count node)))
                    (values node (+ lf-offset (ltree-lfs node))
                            (+ index (ltree-chars node))))
                   (t
                    (recur
                     (- n (+ (ltree-lfs node) (piece-lf-count node)))
                     (right node)
                     (+ lf-offset (+ (ltree-lfs node) (piece-lf-count node)))
                     (+ index (+ (ltree-chars node) (piece-chars node))))))))
    (declare (dynamic-extent #'recur))
    (recur n root 0 0)))

(declaim (ftype (function (t node) idx) node-to-lf))
(defun node-to-lf (node root)
  (declare #.*max-optimize-settings*)
  (loop :until (eq node root)
        :with index :of-type idx = (ltree-lfs node)
        :finally (return index)
        :do (when (right-child-p node)
              (incf index (+ (piece-lf-count (parent node))
                             (ltree-lfs (parent node)))))
            (setf node (parent node))))

;;; piece-table

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
         (string-length-in-octets (length (the (simple-array (unsigned-byte 8))
                                               string-as-octets)))
         (new-fill (+ old-fill string-length-in-octets)))
    (or (< new-fill #.*max-buffer-size*)
        (error "buffer exceeded maximum size ~d!" #.*max-buffer-size*))
    (when (> new-fill old-actual-length)
      (setf (text-buffer-data text-buffer)
            (adjust-array (text-buffer-data text-buffer)
                          (max new-fill (min (* old-actual-length 2)
                                             #.*max-buffer-size*)))))
    (replace (text-buffer-data text-buffer) string-as-octets :start1 old-fill)
    (incf (text-buffer-fill text-buffer) string-length-in-octets)
    string-length-in-octets))

(defstruct (piece-table (:conc-name pt-)
                        (:constructor %make-piece-table))
  "LENGTH is the number of characters in the document.
LINE-COUNT tracks the number of lines in the document.
INITIAL-BUFFER is a TEXT-BUFFER holding the immutable original content of the document.
CHANGE-BUFFER is a TEXT-BUFFER holding any inserted text.
ROOT is the root of the AVL tree of PIECE descriptors.
END-CACHE is the most recently inserted node - used for optimization of (common) sequential inserts."
  (length (required-arg :length) :type idx)
  (line-count (required-arg :line-count) :type idx)
  (initial-buffer (make-text-buffer) :type text-buffer)
  (change-buffer (make-text-buffer) :type text-buffer)
  (root (required-arg :root) :type node)
  (end-cache +sentinel+ :type node)
  (lock nil))

(defmacro with-pt-lock ((piece-table) &body body)
  `(and *lock-piece-table*
        (unwind-protect
             (let ((current-thread (bt:current-thread)))
               (atomics:atomic-update (pt-lock ,piece-table) (constantly current-thread))
               ,@body)
          (atomics:atomic-update (pt-lock ,piece-table) (constantly nil)))))

(declaim (inline pt-piece-buffer
                 pt-fix-ltree-data
                 pt-insert-before pt-insert-after
                 pt-delete-node
                 pt-lf-to-node pt-node-to-index
                 pt-first-node pt-last-node))

(declaim (ftype (function (piece-table node) text-buffer) pt-piece-buffer))
(defun pt-piece-buffer (piece-table node)
  "Returns the data of the text-buffer associated with PIECE."
  (ecase (piece-buffer node)
    (:change-buffer (pt-change-buffer piece-table))
    (:initial-buffer (pt-initial-buffer piece-table))))

(defun pt-fix-ltree-data (piece-table x dchars dlfs)
  (fix-ltree-data x (pt-root piece-table) dchars dlfs))

(defun pt-insert-before (piece-table node x)
  (when-let ((new-root (insert-before node x (pt-root piece-table))))
    (setf (pt-root piece-table) new-root))
  node)

(defun pt-insert-after (piece-table node x)
  (when-let ((new-root (insert-after node x (pt-root piece-table))))
    (setf (pt-root piece-table) new-root))
  node)

(defun pt-delete-node (piece-table node)
  (when-let ((new-root (delete-node node (pt-root piece-table))))
    (setf (pt-root piece-table) new-root))
  node)

(declaim (ftype (function (t idx) (values node idx)) pt-index-to-node))
(defun pt-index-to-node (piece-table index)
  "Returns the node containing INDEX in PIECE-TABLE and its absolute index as multiple
values. Note that the node is chosen to contain the character referred to by INDEX."
  (index-to-node index (pt-root piece-table)))

(declaim (ftype (function (t idx) (values node idx idx)) pt-lf-to-node))
(defun pt-lf-to-node (piece-table linefeed-no)
  "Returns the node containing LINEFEED-NO in PIECE-TABLE, the number of linefeeds seen up
to that point and the index of the found node."
  (multiple-value-bind (node lf-offset index)
      (lf-to-node linefeed-no (pt-root piece-table))
    (values node lf-offset index)))

(defun pt-node-to-index (piece-table node)
  (node-to-index node (pt-root piece-table)))

(defun pt-node-to-lf (piece-table node)
  (node-to-lf node (pt-root piece-table)))

(defun pt-first-node (piece-table)
  (leftmost (pt-root piece-table)))

(defun pt-last-node (piece-table)
  (rightmost (pt-root piece-table)))

;;; utf8 node garbage

(declaim (ftype (function (piece-table node idx idx) (values idx idx))
                utf8-line-to-chars-bytes))
(defun utf8-lines-to-chars-bytes (piece-table node start lines)
  "Returns the character offset of the LINESth line from the byte offset start"
  (declare #.*max-optimize-settings*)
  (loop :with raw = (text-buffer-data (pt-piece-buffer piece-table node))
        :with byte-offset :of-type idx = start
        :with lf-offset :of-type idx
        :with char-offset :of-type idx
        :until (= lf-offset (the idx lines))
        :do (let ((byte (aref raw byte-offset)))
              (cond ((< byte #x80)
                     (when (= byte #.(char-code #\newline))
                       (incf lf-offset))
                     (incf byte-offset))
                    ((< byte #xE0) (incf byte-offset 2))
                    ((< byte #xF0) (incf byte-offset 3))
                    ((< byte #xF8) (incf byte-offset 4))))
            (incf char-offset)
        :finally (return (values char-offset byte-offset))))

(declaim (ftype (function (piece-table node idx idx) (values idx idx)) utf8-count-lfs-bytes))
(defun utf8-count-lfs-bytes (piece-table node start chars)
  (declare #.*max-optimize-settings*)
  (let ((raw (text-buffer-data (pt-piece-buffer piece-table node)))
        (byte-offset start)
        (lf-count 0))
    (declare (type idx lf-count))
    (dotimes (i chars (values byte-offset lf-count))
      (let ((byte (aref raw byte-offset)))
        (cond ((< byte #x80)
               (when (= byte #.(char-code #\newline))
                 (incf lf-count))
               (incf byte-offset))
              ((< byte #xE0) (incf byte-offset 2))
              ((< byte #xF0) (incf byte-offset 3))
              ((< byte #xF8) (incf byte-offset 4)))))))

(declaim (ftype (function (piece-table node idx idx) idx) utf8-count-bytes))
(defun utf8-count-bytes (piece-table node start chars)
  "Note: cache must be valid and call must occur within the dynamic extent of
WITH-CACHE-LOCKED."
  (declare #.*max-optimize-settings*)
  (loop :with raw = (text-buffer-data (pt-piece-buffer piece-table node))
        :with idx :of-type idx = start
        :repeat chars
        :for byte :of-type (unsigned-byte 8) = (aref raw idx)
        :do (cond ((< byte #x80) (incf idx))
                  ((< byte #xE0) (incf idx 2))
                  ((< byte #xF0) (incf idx 3))
                  ((< byte #xF8) (incf idx 4)))
        :finally (return idx)))

(defmacro define-node-fn (name extra return-value &body body)
  `(progn
     (declaim (ftype (function (piece-table node idx idx) (values ,return-value &rest nil))
                     ,name))
     (defun ,name (piece-table node start-offset end-offset ,@extra)
       (let* ((byte-start (utf8-count-bytes piece-table node
                                            (piece-offset node)
                                            start-offset))
              (byte-end (utf8-count-bytes piece-table node
                                          byte-start
                                          (- end-offset start-offset)))
              (buffer (text-buffer-data (pt-piece-buffer piece-table node))))
         ,@body))))

(define-node-fn pt-get-node-string () (simple-array character)
  (babel:octets-to-string buffer :start byte-start :end byte-end :errorp nil))

(define-node-fn pt-count-node-linefeeds () idx
  (count #.(char-code #\newline) buffer :start byte-start :end byte-end))

(defun utf8-char-at (octets byte-offset)
  (declare (type (simple-array (unsigned-byte 8)) octets)
           (type idx byte-offset))
  (let* ((leading-byte (aref octets byte-offset))
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
    (loop :for i :from 1 :below char-byte-size
          :do (setf codepoint (logior (ash codepoint 6)
                                      (logand (aref octets (+ byte-offset i)) #x3F)))
          :finally (return (code-char codepoint)))))

;;; interface

(defvar *pt-chunk-size* (expt 2 13))

;; it's mmap() time
(defun make-piece-table (&key (initial-contents "") initial-stream)
  (let* ((contents-as-octets (if initial-stream ;TODO treat babel decoding errors
                                 (read-stream-content-into-byte-vector initial-stream)
                                 (babel:string-to-octets initial-contents)))
         (content-length (length contents-as-octets)))
    (labels ((first-split-before (octets index)
               (declare (type (simple-array (unsigned-byte 8)) octets))
               (if (= (logand (aref octets index) #xC0) #x80)
                   (first-split-before octets (1- index))
                   index)))
      (declare (dynamic-extent #'first-split-before))
      ;; first create the piece-table with a root
      (let* ((root-end-index (if (<= content-length *pt-chunk-size*)
                                 content-length
                                 (first-split-before contents-as-octets
                                                     *pt-chunk-size*)))
             (root-length
               (babel:vector-size-in-chars contents-as-octets
                                           :start 0
                                           :end root-end-index
                                           :errorp nil))
             (root-linefeeds (count #.(char-code #\newline) contents-as-octets
                                    :start 0 :end root-end-index))
             (root (if (zerop root-length)
                       +sentinel+
                       (make-node :balance-factor 0
                                  :piece-buffer :initial-buffer
                                  :piece-offset 0
                                  :piece-chars root-length
                                  :piece-lf-count root-linefeeds)))
             (piece-table (%make-piece-table :length root-length
                                             :line-count root-linefeeds ; terminating newline
                                             :initial-buffer
                                             (make-text-buffer :data contents-as-octets
                                                               :fill content-length)
                                             :root root)))
        ;; divide up the rest of the contents among several nodes
        (loop :with prev = root
              :with split-point
              :with index = root-end-index
              :with chars
              :with lf-offset
              :while (> (- content-length index) 0)
              :do (setf split-point (if (<= (- content-length index) *pt-chunk-size*)
                                        content-length
                                        (first-split-before contents-as-octets
                                                            (+ index *pt-chunk-size*)))
                        chars (babel:vector-size-in-chars contents-as-octets
                                                          :start index :end split-point
                                                          :errorp nil)
                        lf-offset (count #.(char-code #\newline) contents-as-octets
                                         :start index :end split-point)
                        prev (pt-insert-after piece-table
                                              (make-node :piece-buffer :initial-buffer
                                                         :piece-offset index
                                                         :piece-chars chars
                                                         :piece-lf-count lf-offset)
                                              prev)
                        index split-point)
                  (incf (pt-length piece-table) chars)
                  (incf (pt-line-count piece-table) lf-offset))
        piece-table))))

(defun pt-char (piece-table n)
  (declare #.*max-optimize-settings*
           (type idx n))
  (multiple-value-bind (node node-index)
      (pt-index-to-node piece-table n)
    (let ((raw (text-buffer-data (pt-piece-buffer piece-table node)))
          (byte-offset (utf8-count-bytes piece-table node
                                         (piece-offset node)
                                         (- n node-index))))
      (utf8-char-at raw byte-offset))))

(defun pt-subseq (piece-table start &optional (end (pt-length piece-table)))
  (declare #.*max-optimize-settings*
           (type idx start end))
  (when (= start end)
    (return-from pt-subseq ""))
  ;; only search for START's node first - we can check whether END refers to the same
  ;; node without searching
  (multiple-value-bind (start-node start-node-index)
      (pt-index-to-node piece-table start)
    (when (<= end (+ start-node-index (piece-chars start-node)))
      (return-from pt-subseq
        (pt-get-node-string piece-table start-node
                            (- start start-node-index)
                            (- end start-node-index))))
    ;; we build up the string in piece-sized chunks by traversing between
    ;; start-node and end-node, concatenating their pieces' text to str
    (multiple-value-bind (end-node end-node-index)
        (pt-index-to-node piece-table (1- end))
      (loop :with subseq = (make-array (- end start) :element-type 'character)
              :initially (replace subseq (pt-get-node-string piece-table start-node
                                                             (- start start-node-index)
                                                             (piece-chars start-node)))
            :with subseq-free-index
              :of-type idx = (- (piece-chars start-node) (- start start-node-index))
            :for node = (next-node start-node) :then (next-node node)
            :until (eq node end-node)
            :do (replace subseq
                         (pt-get-node-string piece-table node
                                             0
                                             (piece-chars node))
                         :start1 subseq-free-index)
                (incf subseq-free-index (piece-chars node))
            :finally (return (replace subseq
                                      (pt-get-node-string piece-table end-node
                                                          0
                                                          (- end end-node-index))
                                      :start1 subseq-free-index))))))

(defun pt-line-number-index (piece-table line-number)
  (declare #.*max-optimize-settings*
           (type idx line-number))
  (when (= line-number 1)
    (return-from pt-line-number-index 0))
  (when (= line-number (1+ (pt-line-count piece-table)))
    (return-from pt-line-number-index (pt-length piece-table)))
  ;; find nth linebreak in node - linear, but fairly fast
  (multiple-value-bind (node node-lf-index node-index)
      (pt-lf-to-node piece-table (1- line-number))
    (+ node-index (utf8-lines-to-chars-bytes piece-table node
                                             (piece-offset node)
                                             (- (1- line-number) node-lf-index)))))

;; deprecated
;; (defun pt-index-in-bytes (piece-table index)
;;   (declare #.*max-optimize-settings*
;;            (type idx index))
;;   (when (or (< index 0) (> index (pt-length piece-table)))
;;     (error 'conditions:vico-bad-index :buffer piece-table
;;                                   :bad-index index
;;                                   :bounds (cons 0 (pt-length piece-table))))
;;   (when (zerop index)
;;     (return-from pt-index-in-bytes 0))

;;   (multiple-value-bind (end-node end-node-index)
;;       (pt-index-to-node piece-table (1- index))
;;     (loop
;;       :with bytes :of-type idx
;;       :for node = (pt-first-node piece-table) :then (next-node node)
;;       :until (eq node end-node)
;;       :do (incf bytes (- (utf8-count-bytes piece-table node
;;                                            (piece-offset node)
;;                                            (piece-chars node))
;;                          (piece-offset node)))
;;       :finally (return (+ bytes (- (utf8-count-bytes piece-table end-node
;;                                                      (piece-offset end-node)
;;                                                      (- index end-node-index))
;;                                    (piece-offset end-node)))))))

(defun pt-byte-length (piece-table)
  (pt-length piece-table))

(defun pt-insert (piece-table string index)
  (declare #.*max-optimize-settings*
           (type string string)
           (type idx index))
  (let ((old-change-buffer-size (text-buffer-fill (pt-change-buffer piece-table)))
        (length (length string))
        (lf-offset (count #\newline string)))
    (unless (zerop length)
      (text-buffer-append (pt-change-buffer piece-table) string)
      (cond ((zerop (pt-length piece-table))
             (let* ((new-node (make-node :piece-buffer :change-buffer
                                         :piece-offset old-change-buffer-size
                                         :piece-chars length
                                         :piece-lf-count lf-offset)))
               (setf (pt-end-cache piece-table) new-node)
               (setf (pt-root piece-table) new-node)))
            ;; table is nonempty, insertion before start - cannot be cached
            ((zerop index)
             (let ((first-node (pt-first-node piece-table))
                   (new (make-node :piece-buffer :change-buffer
                                   :piece-offset old-change-buffer-size
                                   :piece-chars length
                                   :piece-lf-count lf-offset)))
               (setf (pt-end-cache piece-table)
                     (pt-insert-before piece-table new first-node))))
            (t
             (multiple-value-bind (node node-index)
                 (pt-index-to-node piece-table (1- index))
               (let ((boundary (= index (+ node-index (piece-chars node)))))
                 ;; common case - appending to (cached) node corresponding to the
                 ;; end of the 'change' piece-table
                 (if (and (eq node (pt-end-cache piece-table)) boundary)
                     (progn
                       (incf (piece-chars node) length)
                       (incf (piece-lf-count node) lf-offset)
                       (pt-fix-ltree-data piece-table node length lf-offset))
                     (let ((new (make-node :piece-buffer :change-buffer
                                           :piece-offset old-change-buffer-size
                                           :piece-chars length
                                           :piece-lf-count lf-offset)))
                       (if boundary ; insertion just after node boundary
                           (setf (pt-end-cache piece-table)
                                 (pt-insert-after piece-table new node))
                           (let* ((right-size
                                    (- (+ node-index (piece-chars node)) index))
                                  (right-offset (utf8-count-bytes piece-table node
                                                                  (piece-offset node)
                                                                  (- index node-index)))
                                  (right-lfs
                                    (pt-count-node-linefeeds piece-table node
                                                             (- index node-index)
                                                             (piece-chars node)))
                                  (new-right
                                    (make-node :piece-buffer (piece-buffer node)
                                               :piece-offset right-offset
                                               :piece-chars right-size
                                               :piece-lf-count right-lfs)))
                             (pt-insert-after piece-table new-right node)
                             (setf (pt-end-cache piece-table)
                                   (pt-insert-after piece-table new node))
                             (decf (piece-chars node) right-size)
                             (decf (piece-lf-count node) right-lfs)
                             (pt-fix-ltree-data piece-table node
                                                (- right-size) (- right-lfs))))))))))
      (incf (pt-length piece-table) length)
      (incf (pt-line-count piece-table) lf-offset))
    (values))) ;corresponds to outermost let

(defun pt-delete-within-piece (piece-table node rel-start rel-end)
  (declare #.*max-optimize-settings*
           (type idx rel-start rel-end))
  (let* ((delta (- rel-end rel-start))
         (start-on-boundary (zerop rel-start))
         (end-on-boundary (= rel-end (piece-chars node))))
    (cond ((and start-on-boundary end-on-boundary)
           (decf (pt-line-count piece-table) (piece-lf-count node))
           (when (eq node (pt-end-cache piece-table))
             (setf (pt-end-cache piece-table) +sentinel+))
           (pt-delete-node piece-table node))
          (start-on-boundary
           (let* ((lf-delta (pt-count-node-linefeeds piece-table node 0 rel-end)))
             (declare (type idx lf-delta))
             (decf (pt-line-count piece-table) lf-delta)
             (pt-fix-ltree-data piece-table node (- delta) (- lf-delta))
             (setf (piece-offset node) (utf8-count-bytes piece-table node
                                                         (piece-offset node)
                                                         delta))
             (decf (piece-chars node) delta)
             (decf (piece-lf-count node) lf-delta)))
          (end-on-boundary
           (let* ((lf-delta (pt-count-node-linefeeds piece-table node rel-start
                                                     (piece-chars node))))
             (declare (type idx lf-delta))
             (when (eq node (pt-end-cache piece-table))
               (setf (pt-end-cache piece-table) +sentinel+))
             (decf (pt-line-count piece-table) lf-delta)
             (pt-fix-ltree-data piece-table node (- (- (piece-chars node) rel-start))
                                (- lf-delta))
             (decf (piece-chars node) delta)
             (decf (piece-lf-count node) lf-delta)))
          (t
           (let* ((before-lf (pt-count-node-linefeeds piece-table node
                                                      0
                                                      rel-start))
                  (after-lf (pt-count-node-linefeeds piece-table node
                                                     rel-end
                                                     (piece-chars node)))
                  (right (pt-insert-after
                          piece-table
                          (make-node :piece-buffer (piece-buffer node)
                                     :piece-offset (utf8-count-bytes piece-table node
                                                                     (piece-offset node)
                                                                     rel-end)
                                     :piece-chars (- (piece-chars node) rel-end)
                                     :piece-lf-count after-lf)
                          node)))
             (declare (type idx before-lf after-lf))
             (when (eq node (pt-end-cache piece-table))
               (setf (pt-end-cache piece-table) right))
             (decf (pt-line-count piece-table) (- (piece-lf-count node)
                                                  (+ before-lf after-lf)))

             (pt-fix-ltree-data piece-table node
                                (- (- (piece-chars node) rel-start))
                                (- (- (piece-lf-count node) before-lf)))
             (setf (piece-chars node) rel-start)
             (setf (piece-lf-count node) before-lf))))))

(defun pt-delete-multiple (piece-table start-node rel-start count)
  (declare #.*max-optimize-settings*
           (type idx rel-start count))
  (loop :with (end-node . end-offset)
          = (loop
              :with remaining :of-type idx = count
                :initially (decf remaining (- (piece-chars start-node) rel-start))
              :for node = (next-node start-node) :then (next-node node)
              :while (and node (> remaining (piece-chars node)))
              :do (decf remaining (piece-chars node))
              :finally (if (null node)
                           (return-from pt-delete-multiple nil)
                           (return (cons node (if (zerop remaining)
                                                  (piece-chars node)
                                                  remaining)))))
        :with new-end-index = (utf8-count-bytes piece-table end-node
                                                (piece-offset end-node)
                                                end-offset)
          :initially (when (eq (next-node start-node) end-node)
                       (loop-finish))
        :with start-offset = (piece-offset start-node)
        :with start-buffer = (piece-buffer start-node)
        :for delete-node = (prev-node end-node)
        :until (and (= (piece-offset delete-node) start-offset)
                    (eq (piece-buffer delete-node) start-buffer))
        :do (decf (pt-line-count piece-table) (piece-lf-count delete-node))
            ;; invalidate as delete-node could be overwritten with some non-end node
            (when (eq delete-node (pt-end-cache piece-table))
              (setf (pt-end-cache piece-table) +sentinel+))
            (pt-delete-node piece-table delete-node)
        :finally ;; from this point DELETE-NODE may (will) contain START-NODE unless vvvvvvv
                 (when (eq delete-node (pt-end-cache piece-table))
                   (setf (pt-end-cache piece-table) +sentinel+))
                 (if (zerop rel-start)
                     (progn
                       (decf (pt-line-count piece-table) (piece-lf-count delete-node))
                       (pt-delete-node piece-table (prev-node end-node)))
                     (let* ((start-node (or delete-node start-node)) ;LOOP-FINISH won't set
                            (lf-delta (pt-count-node-linefeeds piece-table start-node
                                                               rel-start
                                                               (piece-chars start-node))))
                       (declare (type idx lf-delta))
                       (decf (pt-line-count piece-table) lf-delta)
                       (pt-fix-ltree-data piece-table start-node
                                          (- (- (piece-chars start-node) rel-start))
                                          lf-delta)
                       (decf (piece-lf-count start-node) lf-delta)
                       (setf (piece-chars start-node) rel-start)))
                 (if (= end-offset (piece-chars end-node))
                     (progn
                       (decf (pt-line-count piece-table) (piece-lf-count end-node))
                       (when (eq end-node (pt-end-cache piece-table))
                         (setf (pt-end-cache piece-table) +sentinel+))
                       (pt-delete-node piece-table end-node))
                     (let ((lf-delta (pt-count-node-linefeeds piece-table end-node
                                                              0
                                                              end-offset)))
                       (declare (type idx lf-delta))
                       (decf (pt-line-count piece-table) lf-delta)
                       (pt-fix-ltree-data piece-table end-node (- end-offset) (- lf-delta))
                       (decf (piece-lf-count end-node) lf-delta)
                       (decf (piece-chars end-node) end-offset)
                       (setf (piece-offset end-node) new-end-index)))))

(defun pt-delete (piece-table start count)
  (declare #.*max-optimize-settings*
           (type idx start count))
  (let ((end (+ start count)))
    (unless (= start end)
      (multiple-value-bind (start-node start-node-index)
          (pt-index-to-node piece-table start)
        ;; (hopefully) common case when start and end are spanned by 1 node
        (if (<= end (+ start-node-index (piece-chars start-node)))
            (pt-delete-within-piece piece-table start-node
                                    (- start start-node-index) (- end start-node-index))
            (pt-delete-multiple piece-table start-node
                                (- start start-node-index) count))))
    (decf (pt-length piece-table) count)
    (values)))

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
Returns t upon success, nil otherwise."
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
              (setf root (or (insert-after (make-node :piece-chars nodes) node root) root))
              (assert (check-ltree root))
              (assert (check-invariants root))
              (incf nodes))
    (print :delete)
    (loop :repeat n
          :for node = (leftmost root)
          :do (dotimes (i (random nodes))
                (setf node (next-node node)))
              (setf root (or (delete-node node root) root))
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
       (pt-insert piece-table "this" (random (pt-length piece-table)))))))

(defun node-count (piece-table)
  (loop :for counter from 1
        :for node = (leftmost (pt-root piece-table))
          :then (next-node node)
        :while node
        :finally (return counter)))

;; empty insertion case

(let ((piece-table (make-piece-table)))
  (pt-insert piece-table "hi" 0)
  (assert (string= (pt-contents piece-table) "hi")))

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
;; (assert (= (pt-index-in-bytes *piece-table* (pt-length *piece-table*)) 39))

;; (buf:delete *piece-table* 1 1)
;; (buf:delete *piece-table* 2 1)
;; (buf:delete *piece-table* 3 1)

;;(buf:delete *piece-table* 0 3)
;;(assert ())


#+sbcl (require 'sb-sprof)

;;; class

(defvar *lock-piece-table* t
  "Can be dynamically bound in individual threads to prevent locking (you can/need to do
the locking yourself).
IMPORTANT: if you dynamically bind this to NIL and are accessing multiple buffers, be
SURE to lock each one on your first access, then unlock afterwards.")

(defclass piece-table-buffer (buf:buffer)
  ((%piece-table-struct :type piece-table)))

(defmethod initialize-instance :after ((buffer piece-table-buffer)
                                       &key initial-contents initial-stream)
  (setf (slot-value buffer '%piece-table-struct)
        (apply #'make-piece-table (if initial-stream
                                      (list :initial-stream initial-stream)
                                      (when initial-contents
                                        (list :initial-contents initial-contents))))))

;; perhaps we need non-locking variants
(defmethod buf:char-count ((buffer piece-table-buffer))
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (pt-length piece-table))))

(defmethod buf:line-count ((buffer piece-table-buffer))
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (pt-line-count piece-table))))

(defmethod buf:char ((buffer piece-table-buffer) n)
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (or (<= 0 n (1- (pt-length piece-table)))
          (error 'conditions:vico-bad-index :buffer buffer
                                            :bad-index n
                                            :bounds (cons 0 (1- (pt-length piece-table)))))
      (pt-char piece-table n))))

(defmethod buf:subseq ((buffer piece-table-buffer) start &optional end)
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (or (<= end (pt-length piece-table))
          (error 'conditions:vico-bad-index :buffer buffer
                                            :bad-index end
                                            :bounds (cons 0 (pt-length piece-table))))
      (or (<= start end)
          (error 'conditions:vico-bad-index :buffer buffer
                                            :bad-index start
                                            :bounds (cons 0 end)))
      (apply #'pt-subseq piece-table start (when end (list end))))))

(defmethod buf:line-number-index ((buffer piece-table-buffer) line-number)
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (let ((line-count (pt-line-count piece-table)))
        (or (<= 1 line-number (1+ line-count))
            (error 'conditions:vico-bad-line-number :buffer buffer
                                                    :line-number line-number
                                                    :bounds (cons 1 (1+ line-count)))))

      (pt-line-number-index piece-table line-number))))

(defmethod buf:size ((buffer piece-table-buffer))
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (pt-byte-length piece-table))))

(defmethod buf:insert ((buffer piece-table-buffer) string index)
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (or (<= index (pt-length piece-table))
          (error 'conditions:vico-bad-index :buffer buffer
                                            :bad-index index
                                            :bounds (cons 0 (pt-length piece-table))))
      (pt-insert piece-table string index))))

(defmethod buf:delete ((buffer piece-table-buffer) start &optional (count 1))
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (let ((end (+ start count)))
        (or (>= end start)
            (error 'conditions:vico-bad-index :buffer buffer
                                              :bad-index start
                                              :bounds (cons 0 end)))
        (or (<= end (pt-length piece-table))
            (error 'conditions:vico-bad-index :buffer buffer
                                              :bad-index end
                                              :bounds (cons 0 (pt-length piece-table)))))
      (pt-delete piece-table start count))))

(defstruct (piece-table-cursor (:conc-name nil))
  buffer
  dirty-p
  (node +sentinel+ :type node)
  (lf-offset 0 :type idx)
  (byte-offset 0 :type idx)
  (char-offset 0 :type idx))

(defmethod buf:cursor-buffer ((cursor piece-table-cursor))
  (buffer cursor))

(defmethod buf:dirty-cursor ((cursor piece-table-cursor))
  (setf (dirty-p cursor) t))

(defmethod buf:make-cursor ((buffer piece-table-buffer) index)
  (let ((piece-table (slot-value buffer '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (or (<= 0 index (1- (pt-length piece-table)))
          (error 'conditions:vico-bad-index
                 :buffer buffer
                 :bad-index index
                 :bounds (cons 0 (1- (pt-length piece-table)))))
      (multiple-value-bind (node node-index)
          (pt-index-to-node piece-table index)
        (multiple-value-bind (byte-offset lf-count)
            (utf8-count-lfs-bytes piece-table node
                                  (piece-offset node)
                                  (- index node-index))
          (make-piece-table-cursor :buffer buffer
                                   :node node
                                   :lf-offset lf-count
                                   :byte-offset byte-offset
                                   :char-offset (- index node-index)))))))

(defmethod buf:copy-cursor ((cursor piece-table-cursor))
  (copy-piece-table-cursor cursor))

;; (defmethod buf:cursor-valid-p ((cursor piece-table-cursor))
;;   (let ((piece-table (slot-value (buffer cursor) '%piece-table-struct)))
;;     (with-pt-lock (piece-table)
;;       (update-cursor cursor)
;;       (if (= (buf:index-at cursor) (pt-length piece-table))
;;           (error "~a off end?" cursor)
;;           (let ((new (buf:make-cursor (buffer cursor) (buf:index-at cursor))))
;;             (and (eq (node cursor) (node new))
;;                  (= (char-offset cursor) (char-offset new))
;;                  (= (byte-offset cursor) (byte-offset new))
;;                  (= (lf-offset cursor) (lf-offset new))))))))

(defmethod (setf buf:index-at) (new-value (cursor piece-table-cursor))
  (setf (char-offset cursor) new-value
        (dirty-p cursor) t))

(defmethod update-cursor ((cursor piece-table-cursor))
  (when (zerop (piece-chars (node cursor))) ;XXX
    (setf (dirty-p cursor) t)
    (incf (char-offset cursor) (piece-offset (node cursor))))
  (cond ((dirty-p cursor)
         (let ((new (buf:make-cursor (buffer cursor) (char-offset cursor))))
           (setf (char-offset cursor) (char-offset new)
                 (byte-offset cursor) (byte-offset new)
                 (node cursor) (node new)
                 (lf-offset cursor) (lf-offset new)
                 (dirty-p cursor) nil)))
        ((and (= (char-offset cursor) (piece-chars (node cursor)))
              (next-node (node cursor)))
         (let ((node (node cursor)))
           (setf (node cursor) (next-node node)
                 (char-offset cursor) 0
                 (byte-offset cursor) (piece-offset (node cursor))
                 (lf-offset cursor) 0)))))

(defmethod buf:char-at ((cursor piece-table-cursor))
  (let ((piece-table (slot-value (buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (let ((raw (text-buffer-data (pt-piece-buffer piece-table (node cursor)))))
        (utf8-char-at raw (byte-offset cursor))))))

;; O(log n) pieces
(defmethod buf:index-at ((cursor piece-table-cursor))
  (let ((piece-table (slot-value (buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (+ (pt-node-to-index piece-table (node cursor)) (char-offset cursor)))))

(declaim (inline pt-cursor-next-in-node))
(defun pt-cursor-next-in-node (piece-table cursor chars)
  (declare #.*max-optimize-settings*
           (type idx chars))
  (let ((node (node cursor)))
    (multiple-value-bind (new-byte-offset lf-count)
        (utf8-count-lfs-bytes piece-table node (byte-offset cursor) chars)
      (setf (byte-offset cursor) new-byte-offset)
      (incf (the idx (char-offset cursor)) chars)
      (incf (the idx (lf-offset cursor)) lf-count))))

(defmethod buf:cursor-next ((cursor piece-table-cursor) &optional (count 1))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (let ((old-node (node cursor)))
        (if (< (+ (char-offset cursor) count) (piece-chars old-node))
            (pt-cursor-next-in-node piece-table cursor count)
            (loop :initially (decf remaining-chars (- (piece-chars old-node)
                                                      (char-offset cursor)))
                  :for new-node = (next-node (node cursor)) :then (next-node new-node)
                  :with remaining-chars = count
                  :while (and new-node (>= remaining-chars (piece-chars new-node)))
                  :do (decf remaining-chars (piece-chars new-node))
                  :finally (or new-node
                               (error 'conditions:vico-bad-index
                                      :buffer (buf:cursor-buffer cursor)
                                      :bad-index (+ (buf:index-at cursor) count)
                                      :bounds (cons 0 (1- (pt-length piece-table)))))
                           (setf (node cursor) new-node
                                 (byte-offset cursor) (piece-offset new-node)
                                 (char-offset cursor) 0
                                 (lf-offset cursor) 0)
                           (pt-cursor-next-in-node piece-table cursor remaining-chars))))
      cursor)))

(declaim (inline pt-cursor-prev-in-node))
(defun pt-cursor-prev-in-node (piece-table cursor chars)
  (declare #.*max-optimize-settings*
           (type idx chars))
  (loop :with node = (node cursor)
        :with raw = (text-buffer-data (pt-piece-buffer piece-table node))
        :with old-char-offset :of-type idx = (char-offset cursor)
        :until (= (the idx (char-offset cursor)) (- old-char-offset chars))
        :do (decf (the idx (byte-offset cursor)))
            (let ((byte (aref raw (byte-offset cursor))))
              (unless (= (logand byte #xC0) #x80)
                (decf (the idx (char-offset cursor)))
                (when (= byte #.(char-code #\newline))
                  (decf (the idx (lf-offset cursor))))))))

(defmethod buf:cursor-prev ((cursor piece-table-cursor) &optional (count 1))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (if (>= (char-offset cursor) count)
          (pt-cursor-prev-in-node piece-table cursor count)
          (loop :initially (decf remaining-chars (char-offset cursor))
                :for new-node = (prev-node (node cursor)) :then (prev-node new-node)
                :with remaining-chars = count
                :while (and new-node (> remaining-chars (piece-chars new-node)))
                :do (decf remaining-chars (piece-chars new-node))
                :finally (or new-node
                             (error 'conditions:vico-bad-index
                                    :buffer (buf:cursor-buffer cursor)
                                    :bad-index (- (buf:index-at cursor) count)
                                    :bounds (cons 0 (1- (pt-length piece-table)))))
                         (setf (node cursor) new-node
                               (byte-offset cursor) (utf8-count-bytes
                                                     piece-table new-node
                                                     (piece-offset new-node)
                                                     (piece-chars new-node))
                               (char-offset cursor) (piece-chars new-node)
                               (lf-offset cursor) (piece-lf-count new-node))
                         (pt-cursor-prev-in-node piece-table cursor remaining-chars)))
      cursor)))

(defmethod buf:line-at ((cursor piece-table-cursor))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (+ 1 (pt-node-to-lf piece-table (node cursor)) (lf-offset cursor)))))

(declaim (inline pt-cursor-next-line-in-node))
(defun pt-cursor-next-line-in-node (piece-table cursor lines)
  (declare #.*max-optimize-settings*
           (type idx lines))
  (let ((node (node cursor)))
    (multiple-value-bind (chars-skipped new-byte-offset)
        (utf8-lines-to-chars-bytes piece-table node (byte-offset cursor) lines)
      (setf (byte-offset cursor) new-byte-offset)
      (incf (the idx (char-offset cursor)) (the idx chars-skipped))
      (incf (the idx (lf-offset cursor)) lines))))

(defmethod buf:cursor-next-line ((cursor piece-table-cursor) &optional (count 1))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (let* ((buffer (buffer cursor))
             (new-line (+ (buf:line-at cursor) count))
             (new (ignore-errors
                   (buf:make-cursor buffer (buf:line-number-index buffer new-line))
                   ;; (conditions:vico-bad-index (e)
                   ;;   (declare (ignore e))
                   ;;   (print :damn))
                   )))
        (or (<= new-line (pt-line-count piece-table))
            (error 'conditions:vico-bad-line-number
                   :buffer (buf:cursor-buffer cursor)
                   :line-number new-line
                   :bounds (cons 1 (pt-line-count piece-table))))
        (setf (char-offset cursor) (char-offset new)
              (byte-offset cursor) (byte-offset new)
              (node cursor) (node new)
              (lf-offset cursor) (lf-offset new)))
      cursor)))

(declaim (inline pt-cursor-prev-line-in-node))
(defun pt-cursor-prev-line-in-node (piece-table cursor lines)
  (declare #.*max-optimize-settings*
           (type idx lines))
  (loop :with node = (node cursor)
        :with raw = (text-buffer-data (pt-piece-buffer piece-table node))
        :with old-lf-offset :of-type idx = (lf-offset cursor)
        :until (< (the idx (lf-offset cursor)) (- old-lf-offset lines))
        :do (decf (the idx (byte-offset cursor)))
            (let ((byte (aref raw (byte-offset cursor))))
              (unless (= (logand byte #xC0) #x80)
                (decf (the idx (char-offset cursor)))
                (when (= byte #.(char-code #\newline))
                  (decf (the idx (lf-offset cursor))))))
        :finally (incf (char-offset cursor)) ;back up (forwards) over the newline we skipped
                 (incf (byte-offset cursor))
                 (incf (lf-offset cursor))))

(defmethod buf:cursor-prev-line ((cursor piece-table-cursor) &optional (count 1))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (let* ((buffer (buffer cursor))
             (new-line (- (buf:line-at cursor) count))
             (new (ignore-errors
                   (buf:make-cursor buffer (buf:line-number-index buffer new-line))
                   ;; (conditions:vico-bad-index (e)
                   ;;   (declare (ignore e))
                   ;;   (print :damp))
                   )))
        (or (plusp new-line)
            (error 'conditions:vico-bad-line-number
                   :buffer (buf:cursor-buffer cursor)
                   :line-number new-line
                   :bounds (cons 1 (pt-line-count piece-table))))
        (setf (char-offset cursor) (char-offset new)
              (byte-offset cursor) (byte-offset new)
              (node cursor) (node new)
              (lf-offset cursor) (lf-offset new)))
      cursor)))

;; (defmethod buf:cursor-bol ((cursor piece-table-cursor))
;;   (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
;;     (with-pt-lock (piece-table)
;;       (update-cursor cursor)
;;       (let* ((buffer (buffer cursor))
;;              (new (buf:make-cursor buffer (buf:line-number-index buffer
;;                                                                  (buf:line-at cursor)))))
;;         (setf (char-offset cursor) (char-offset new)
;;               (byte-offset cursor) (byte-offset new)
;;               (node cursor) (node new)
;;               (lf-offset cursor) (lf-offset new))
;;         cursor))))

(defmethod buf:insert-at ((cursor piece-table-cursor) string)
  (declare (type string string))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (let ((old-change-buffer-size (text-buffer-fill (pt-change-buffer piece-table)))
            (length (length string))
            (lf-offset (count #\newline string))
            (cursor-offset (char-offset cursor)))
        (unless (zerop length)
          (text-buffer-append (pt-change-buffer piece-table) string)
          (cond ((zerop (pt-length piece-table))
                 (let* ((new-node (make-node :piece-buffer :change-buffer
                                             :piece-offset old-change-buffer-size
                                             :piece-chars length
                                             :piece-lf-count lf-offset)))
                   (setf (pt-end-cache piece-table) new-node)
                   (setf (pt-root piece-table) new-node)))
                ;; table is nonempty, insertion before start - cannot be cached
                ((and (null (prev-node (node cursor))) (zerop cursor-offset))
                 (let* ((first-node (pt-first-node piece-table))
                        (new (make-node :piece-buffer :change-buffer
                                        :piece-offset old-change-buffer-size
                                        :piece-chars length
                                        :piece-lf-count lf-offset))
                        (new-node (pt-insert-before piece-table new first-node)))
                   (setf (pt-end-cache piece-table) new-node
                         (node cursor) new-node
                         (byte-offset cursor) (piece-offset new-node))))
                (t
                 (let ((node (node cursor))
                       (boundary (zerop cursor-offset)))
                   ;; common case - appending to (cached) node corresponding to the
                   ;; end of the 'change' buffer
                   (if (and boundary (eq (prev-node node) (pt-end-cache piece-table)))
                       (let ((node (prev-node node)))
                         (setf (node cursor) node
                               (byte-offset cursor) (utf8-count-bytes piece-table node
                                                                      (piece-offset node)
                                                                      (piece-chars node))
                               (char-offset cursor) (piece-chars node)
                               (lf-offset cursor) (piece-lf-count node))
                         (incf (piece-chars node) length)
                         (incf (piece-lf-count node) lf-offset)
                         (pt-fix-ltree-data piece-table node length lf-offset))
                       (let ((new (make-node :piece-buffer :change-buffer
                                             :piece-offset old-change-buffer-size
                                             :piece-chars length
                                             :piece-lf-count lf-offset)))
                         (if boundary ; insertion just after node boundary
                             (let ((new (pt-insert-after piece-table new (prev-node node))))
                               (setf (pt-end-cache piece-table) new)
                               (setf (node cursor) new
                                     (byte-offset cursor) (piece-offset new)))
                             (let* ((right-size (- (piece-chars node) cursor-offset))
                                    (right-offset (utf8-count-bytes piece-table node
                                                                    (piece-offset node)
                                                                    cursor-offset))
                                    (right-lfs
                                      (pt-count-node-linefeeds piece-table node
                                                               cursor-offset
                                                               (piece-chars node)))
                                    (new-right
                                      (make-node :piece-buffer (piece-buffer node)
                                                 :piece-offset right-offset
                                                 :piece-chars right-size
                                                 :piece-lf-count right-lfs)))
                               (pt-insert-after piece-table new-right node)
                               (setf (pt-end-cache piece-table)
                                     (pt-insert-after piece-table new node))
                               (decf (piece-chars node) right-size)
                               (decf (piece-lf-count node) right-lfs)
                               (pt-fix-ltree-data piece-table node
                                                  (- right-size) (- right-lfs))
                               (setf (node cursor) new
                                     (byte-offset cursor) (piece-offset new)
                                     (char-offset cursor) 0
                                     (lf-offset cursor) 0))))))))
          (incf (pt-length piece-table) length)
          (incf (pt-line-count piece-table) lf-offset))
        (values)))))

(defmethod buf:delete-at ((cursor piece-table-cursor) &optional (count 1))
  (let ((piece-table (slot-value (buf:cursor-buffer cursor) '%piece-table-struct)))
    (with-pt-lock (piece-table)
      (update-cursor cursor)
      (let ((start-node (node cursor))
            (start-boundary-p (zerop (char-offset cursor)))
            saved-index)
        ;; later node deleted, prev maybe copied (1)
        (when (and start-boundary-p (prev-node (node cursor)))
          (setf saved-index (+ (pt-node-to-index piece-table (node cursor))
                               (char-offset cursor))))
        (if (<= (+ (char-offset cursor) count) (piece-chars start-node))
            (if (and (= (+ (char-offset cursor) count) (piece-chars start-node))
                     (null (next-node start-node)))
                (error 'conditions:vico-bad-index
                       :buffer (buf:cursor-buffer cursor)
                       :bad-index (pt-length piece-table)
                       :bounds (cons 0 (1- (pt-length piece-table))))
                (pt-delete-within-piece piece-table start-node
                                        (char-offset cursor)
                                        (+ (char-offset cursor) count)))
            (or (pt-delete-multiple piece-table start-node (char-offset cursor) count)
                (error 'conditions:vico-bad-index
                       :buffer (buf:cursor-buffer cursor)
                       :bad-index (+ (buf:index-at cursor) count)
                       :bounds (cons 0 (1- (pt-length piece-table))))))
        (when start-boundary-p
          (if saved-index
              (setf (char-offset cursor) saved-index
                    (dirty-p cursor) t)
              ;; optimization: deletion at index 0, we know exactly where it is now
              (setf (node cursor) (pt-first-node piece-table)
                    (byte-offset cursor) (piece-offset (node cursor)))))
        (decf (pt-length piece-table) count)
        cursor))))

;; (loop :for counter from 1
;;       :for node = (leftmost (pt-root (slot-value (first (vico-core.command-loop:buffers vico-core.command-loop:*editor*)) '%piece-table-struct)))
;;         :then (next-node node)
;;       :while node
;;       :when (zerop (piece-chars node))
;;         :do (print counter) (loop-finish)
;;       :finally (return counter))

;; (pt-tree (slot-value (first (vico-core.command-loop:buffers vico-core.command-loop:*editor*)) '%piece-table-struct))
;; (defparameter test
;;   (buf:make-cursor (first (vico-core.command-loop:buffers vico-core.command-loop:*editor*)) 18827))

;; (buf:size (first (vico-core.command-loop:buffers vico-core.command-loop:*editor*)))
;; (vico-core.ui:window-point (vico-core.ui:focused-window (first (vico-core.command-loop:frontends vico-core.command-loop:*editor*))))

;;;tests

(defun cursor-forwards-by-n (piece-table n)
  (loop :for i :below (buf:size piece-table) :by n
        :with curs = (buf:make-cursor piece-table 0)
        :for c1 = (buf:char-at curs)
        :for c2 = (buf:char piece-table i)
        :do (assert (char= c1 c2))
            (unless (>= (+ n (buf:index-at curs)) (buf:size piece-table))
              (buf:cursor-next curs n))))

(defun cursor-backwards-by-n (piece-table n)
  (loop :for i :downfrom (1- (buf:size piece-table)) :to 0 :by n
        :with curs = (buf:make-cursor piece-table (1- (buf:size piece-table)))
        :for c1 = (buf:char-at curs)
        :for c2 = (buf:char piece-table i)
        :do (assert (char= c1 c2))
            (unless (< (- (buf:index-at curs) n) 0)
              (buf:cursor-prev curs n))))

(defun cursor-forwards-by-n-lines (piece-table n)
  (loop :for i :from 1 to (buf:line-count piece-table) :by n
        :with curs = (buf:make-cursor piece-table 0)
        :for c1 = (buf:char-at curs)
        :for c2 = (buf:char piece-table (buf:line-number-index piece-table i))
        :do (assert (char= c1 c2))
            (unless (> (+ n (buf:line-at curs)) (buf:line-count piece-table))
              (buf:cursor-next-line curs n))))

(defun cursor-backwards-by-n-lines (piece-table n)
  (loop :with line-count = (buf:line-count piece-table)
        :for i :downfrom line-count :to 1 :by n
        :with curs = (buf:make-cursor piece-table
                                      (buf:line-number-index piece-table line-count))
        :for c1 = (buf:char-at curs)
        :for c2 = (buf:char piece-table (buf:line-number-index piece-table i))
        :do (assert (char= c1 c2))
            (unless (< (- (buf:line-at curs) n) 1)
              (buf:cursor-prev-line curs n))))
