;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Piece table implementation
;;
;; thanks to:
;;
;; *Jürgen Böhms Heimatseiten* for his red-black tree implementation available at
;; (http://www.aviduratas.de/lisp/progs/rb-trees-struct-1-01.lisp)
;; ---
;; *Abiword* (fast, unbloated word processing https://www.abisource.com/) for inspiration
;;
;; TODO cleanup, write tests and split into separate library
;;

(defpackage vico-core.buffer.piece-table
  (:use :cl :alexandria)
  (:import-from :vico-core.buffer :*max-buffer-size*)
  (:local-nicknames (:buffer :vico-core.buffer))
  (:export #:piece-table-buffer))
(in-package :vico-core.buffer.piece-table)

(defun required-arg (arg)
  (error "struct arg ~A is required" arg))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *max-optimize-settings*
    '(optimize (speed 3) (safety 1) (debug 0) (space 0) (compilation-speed 0)))
  (deftype idx () '(integer 0 #.*max-buffer-size*))

;;;
;;; piece descriptors
;;;

  (defconstant +change-buffer+ 0)
  (defconstant +initial-buffer+ 1)

  (defstruct piece
    "PIECE is a descriptor that refers to a segment of text in a BUFFER.
OFFSET represents the offset of the first character referred to by the piece (into the
piece's corresponding BUFFER).
SIZE tracks the length of the text referred to by the piece.
LF-COUNT tracks the number of linefeed characters in the text referred to by the piece.
BUFFER is bit indicating which buffer the piece corresponds to."
    (offset 0 :type idx)
    (size 0 :type idx)
    (lf-count 0 :type idx)
    (buffer +change-buffer+ :type bit))

  (defmethod make-load-form ((obj piece) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots obj))

;;;
;;; binary-tree node
;;;

  (defconstant +black+ 0)
  (defconstant +red+ 1)

  (defstruct (node (:conc-name nil)
                   (:constructor %make-node)
                   (:print-object %print-node))
    "NODE is the building block of the (red-black) binary tree that holds the PIECE
descriptors that constitute a document.
PARENT - parent node.
LEFT - left child node.
RIGHT - right child node.
COLOR - color parity of the node. New nodes are red by default.
PIECE - the piece corresponding to this node
LTREE-SIZE - tracks the total size of all pieces in the node's left subtree. Used to
guarantee O(log n) (n being number of nodes) searches for a given offset.
LTREE-LFS - similar to LTREE-SIZE above but with linefeed characters"
    parent ; node pointers
    left
    right
    (piece (required-arg 'piece))
    (color +red+ :type bit)
    (ltree-size 0 :type idx)
    (ltree-lfs 0 :type idx))

  (defmethod make-load-form ((obj node) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots obj)))

(define-constant +sentinel-piece+
    (make-piece :size #.*max-buffer-size* :lf-count #.*max-buffer-size*)
  :test (constantly t))

(define-constant +sentinel+ (%make-node :color +black+ :piece +sentinel-piece+)
  :test (constantly t)
  :documentation "Sentinel node - represents the empty leaves of the tree - simplifies
deletion.")
(setf (left +sentinel+) (setf (right +sentinel+) +sentinel+))

(declaim (inline make-node))
(defun make-node (piece &key (color +red+))
  (%make-node :color color :left +sentinel+ :right +sentinel+ :piece piece))

;;;
;;; tree utilities
;;;

(declaim (inline node-null
                 right-child-p left-child-p
                 blacken redden
                 blackp redp
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

(defun blacken (node)
  (setf (color node) +black+))

(defun redden (node)
  (setf (color node) +red+))

(defun redp (node)
  (= (color node) +red+))

(defun blackp (node)
  (= (color node) +black+))

(defun grandparent (node)
  (parent (parent node)))

;; node printing function - needs redp

(defun %print-node (node s)
  (let ((color (if (redp node) "red" "black"))
        (piece (piece node))
        (ltree-size (ltree-size node)))
    (format s "#S(node :color ~A :piece ~A :lsize ~d)"
            color piece ltree-size)))

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
      (loop :while (parent x)
            :do (if (right-child-p x)
                    (return-from prev-node (parent x))
                    (setf x (parent x))))))

(defun next-node (x)
  (if (not (node-null (right x)))
      (leftmost (right x))
      (loop :while (parent x)
            :do (if (left-child-p x)
                    (return-from next-node (parent x))
                    (setf x (parent x))))))

(declaim (ftype (function (node) idx) calculate-size calculate-lfs))

(defun calculate-size (x)
  (declare #.*max-optimize-settings*)
  (labels ((recur (x res)
             (declare (type idx res))
             (if (node-null x)
                 res
                 (recur (right x)
                        (+ res (piece-size (piece x)) (ltree-size x))))))
    (recur x 0)))

(defun calculate-lfs (x)
  (declare #.*max-optimize-settings*)
  (labels ((recur (x res)
             (declare (type idx res))
             (if (node-null x)
                 res
                 (recur (right x)
                        (+ res (piece-lf-count (piece x)) (ltree-lfs x))))))
    (recur x 0)))

;;;
;;; red-black tree rebalancing
;;;

(defun rotate-left (x)
  (declare #.*max-optimize-settings*)
  (cond ((node-null x) nil)
        (t
         (let* ((y (right x))
                (b (left y)))
           (unless (node-null x)
             (incf (ltree-size y) (+ (piece-size (piece x)) (ltree-size x)))
             (incf (ltree-lfs y) (+ (piece-lf-count (piece x)) (ltree-lfs x))))
           (setf (right x) b)
           (setf (left y) x)
           (unless (node-null b)
             (setf (parent b) x))
           (when (null (parent x))
             (if (not (node-null y))
                 (setf (parent y) nil))
             (setf (parent x) y)
             (return-from rotate-left y))
           (setf (parent y) (parent x))
           (if (left-child-p x)
               (setf (left (parent x)) y)
               (setf (right (parent x)) y))
           (setf (parent x) y)
           nil))))

(defun rotate-right (y)
  (declare #.*max-optimize-settings*)
  (cond ((node-null y) nil)
        (t
         (let* ((x (left y))
                (b (right x)))
           (unless (node-null x)
             (decf (ltree-size y) (+ (piece-size (piece x)) (ltree-size x)))
             (decf (ltree-lfs y) (+ (piece-lf-count (piece x)) (ltree-lfs x))))
           (setf (left y) b)
           (setf (right x) y)
           (unless (node-null b)
             (setf (parent b) y))
           (when (null (parent y))
             (if (not (node-null x))
                 (setf (parent x) nil))
             (setf (parent y) x)
             (return-from rotate-right x))
           (setf (parent x) (parent y))
           (if (right-child-p y)
               (setf (right (parent y)) x)
               (setf (left (parent y)) x))
           (setf (parent y) x)
           nil))))

(defun fix-ltree-data (x root)
  "Fix the left-subtree metadata of x's parents."
  (declare #.*max-optimize-settings*)
  (unless (eq x root)
    (let ((delta 0)
          (lf-delta 0))
      ;; traverse up to the first node containing x in its left subtree to get delta
      (loop :while (and (not (eq x root)) (right-child-p x))
            :do (setf x (parent x))
            :finally (when (eq x root)
                       (return-from fix-ltree-data))
                     (setf x (parent x)))
      ;; Go up towards root, propagating the change to x's parents
      ;; note: we operate on (parent root) to ensure that LEFT-CHILD-P is always
      ;; valid while also treating ROOT properly
      (loop :initially (incf (ltree-size x) delta)
                       (incf (ltree-lfs x) lf-delta)
            :with delta = (- (calculate-size (left x)) (ltree-size x))
            :with lf-delta = (- (calculate-lfs (left x)) (ltree-lfs x))
            :while (not (eq x root))
            :do (when (left-child-p x)
                  (incf (ltree-size (parent x)) delta)
                  (incf (ltree-lfs (parent x)) lf-delta))
                (setf x (parent x)))
      (values delta lf-delta))))

(defun fix-insert (pivot root)
  (declare #.*max-optimize-settings*)
  (fix-ltree-data pivot root)
  (let ((new-root (loop
                    :with y = nil
                    :with x = pivot
                    :with res = nil
                    :with new-root = root
                    :while (and (not (eq x new-root)) (redp (parent x)))
                    :finally (return new-root)
                    :do (cond ((eq (parent x) (left (grandparent x)))
                               (setf y (right (grandparent x)))
                               (cond ((redp y)
                                      (blacken (parent x))
                                      (blacken y)
                                      (redden (grandparent x))
                                      (setf x (grandparent x)))
                                     (t
                                      (blacken (parent x))
                                      (redden (grandparent x))
                                      (setf res (rotate-right (grandparent x)))
                                      (when res
                                        (setf new-root res)))))
                              (t
                               (setf y (left (grandparent x)))
                               (cond ((redp y)
                                      (blacken (parent x))
                                      (blacken y)
                                      (redden (grandparent x))
                                      (setf x (grandparent x)))
                                     (t
                                      (when (left-child-p x)
                                        (setf x (parent x))
                                        (setf res (rotate-right x))
                                        (when res
                                          (setf new-root res)))
                                      (blacken (parent x))
                                      (redden (grandparent x))
                                      (setf res (rotate-left (grandparent x)))
                                      (when res
                                        (setf new-root res)))))))))
    (blacken new-root)
    new-root))

(defun fix-delete (pivot parpivot root)
  (declare #.*max-optimize-settings*)
  (let (newpiv)
    (loop
      :while (not (eq pivot root))
      :finally (return root)
      :do (setf newpiv t)
          (if (eq (left parpivot) pivot)
              (let ((alpha parpivot)
                    (beta)
                    (gamma)
                    (delta))
                (cond ((redp alpha)
                       (setf beta (right alpha))
                       (setf gamma (left beta))
                       (cond ((blackp gamma)       ;1a
                              (setf root (or (rotate-left alpha) root)))
                             (t
                              (setf root (or (rotate-right beta) root))  ; 1b
                              (setf root (or (rotate-left alpha) root))
                              (blacken alpha)))
                       (return-from fix-delete root))
                      ((blackp alpha)
                       (setf beta (right alpha))
                       (cond ((blackp beta)
                              (setf gamma (left beta))
                              (setf delta (right beta))
                              (cond ((redp gamma)
                                     (cond ((redp delta)
                                            (redden beta)
                                            (blacken gamma)
                                            (blacken delta)) ; 2c -> 3
                                           (t
                                            (setf root (or (rotate-right beta) root))
                                            (setf root (or (rotate-left alpha) root))
                                            (blacken gamma)
                                            (return-from fix-delete root)))) ; 2b1
                                    ;; gamma is black, now decide if delta is black
                                    ;; too (2a) or red (2b2)
                                    (t
                                     (cond ((redp delta)
                                            (setf root (or (rotate-left alpha) root))
                                            (blacken delta)
                                            (return-from fix-delete root)) ; 2b2
                                           (t ; now comes 2a
                                            (redden beta)
                                            (setf newpiv alpha))))))
                             (t ; this means beta is red, this gives cases 3a and 3b
                              (setf gamma (left beta))
                              (setf delta (left gamma))
                              (cond ((redp delta)   ; this is 3b
                                     (setf root (or (rotate-left alpha) root))
                                     (setf root (or (rotate-right gamma) root))
                                     (setf root (or (rotate-left alpha) root))
                                     (blacken beta)
                                     (return-from fix-delete root))
                                    (t  ; this is 3a
                                     (setf root (or (rotate-left alpha) root))
                                     (setf root (or (rotate-left alpha) root))
                                     (redden alpha)
                                     (blacken beta)
                                     (return-from fix-delete root))))))))

              ;; the following code is dual under left-right
              (let ((alpha parpivot)
                    (beta)
                    (gamma)
                    (delta))
                (cond ((redp alpha)
                       (setf beta (left alpha))
                       (setf gamma (right beta))
                       (cond ((blackp gamma)       ;1a
                              (setf root (or (rotate-right alpha) root)))
                             (t
                              (setf root (or (rotate-left beta) root))  ; 1b
                              (setf root (or (rotate-right alpha) root))
                              (blacken alpha)))
                       (return-from fix-delete root))
                      ((blackp alpha)
                       (setf beta (left alpha))
                       (cond ((blackp beta)
                              (setf gamma (right beta))
                              (setf delta (left beta))
                              (cond ((redp gamma)
                                     (cond ((redp delta)
                                            (redden beta)
                                            (blacken gamma)
                                            (blacken delta)) ; 2c -> 3
                                           (t
                                            (setf root (or (rotate-left beta) root))
                                            (setf root (or (rotate-right alpha) root))
                                            (blacken gamma)
                                            (return-from fix-delete root)))) ; 2b1
                                    ;; gamma is black, is delta is black too (2a)?
                                    ;; or red (2b2)
                                    (t
                                     (cond ((redp delta)
                                            (setf root (or (rotate-right alpha) root))
                                            (blacken delta)
                                            (return-from fix-delete root)) ; 2b2
                                           (t ; now comes 2a
                                            (redden beta)
                                            (setf newpiv alpha))))))
                             (t ; this means beta is red, this gives cases 3a and 3b
                              (setf gamma (right beta))
                              (setf delta (right gamma))
                              (cond ((redp delta)   ; this is 3b
                                     (setf root (or (rotate-right alpha) root))
                                     (setf root (or (rotate-left gamma) root))
                                     (setf root (or (rotate-right alpha) root))
                                     (blacken beta)
                                     (return-from fix-delete root))
                                    (t  ; this is 3a
                                     (setf root (or (rotate-right alpha) root))
                                     (setf root (or (rotate-right alpha) root))
                                     (redden alpha)
                                     (blacken beta)
                                     (return-from fix-delete root)))))))))
          (unless (eq newpiv t)
            (setf pivot newpiv)
            (setf parpivot (parent pivot))))))

;;;
;;; tree algorithms
;;;

(defun insert-before (piece x root)
  "Inserts PIECE directly before X in the tree rooted at ROOT.
Returns the new tree root and the inserted node as multiple values."
  (declare #.*max-optimize-settings*)
  (let ((z (make-node piece)))
    (cond ((node-null (left x))
           (setf (left x) z)
           (setf (parent z) x))
          (t
           (setf x (rightmost (left x)))
           (setf (right x) z)
           (setf (parent z) x)))
    (setf root (fix-insert z root))
    (values root z)))

(defun insert-after (piece x root)
  "Inserts PIECE directly after X in the tree rooted at ROOT.
Returns the new tree root and the inserted node as multiple values."
  (declare #.*max-optimize-settings*)
  (let ((z (make-node piece)))
    (cond ((node-null (right x))
           (setf (right x) z)
           (setf (parent z) x))
          (t
           (setf x (leftmost (right x)))
           (setf (left x) z)
           (setf (parent z) x)))
    (setf root (fix-insert z root))
    (values root z)))

(defun delete-node (y root)
  "Delete the node Y from the tree rooted at ROOT.
Informative comments adapted from original source."
  (declare #.*max-optimize-settings*)
  (let (x z) ; x is 'actually' deleted, z is x's child (may be +sentinel+)
    (setf (piece-size (piece y)) 0) ; y's *piece* will be deleted
    (fix-ltree-data y root)
    (cond ((node-null (left y))
           (setf x y)
           (setf z (right y)))
          ((node-null (right y))
           (setf x y)
           (setf z (left y)))
          (t
           (setf x (leftmost (right y)))
           (setf (piece y) (piece x))
           ;; do a little manual left subtree metadata fixup from (parent x) up
           ;; to (right y) as x will be 'deleted' and its piece stored in y
           (loop :for w = x :then (parent w)
                 :until (eq w (right y))
                 :do (decf (ltree-size (parent w)) (piece-size (piece x)))
                     (decf (ltree-lfs (parent w)) (piece-lf-count (piece x))))
           (setf z (right x))))
    ;; case when x is root
    (when (eq x root)
      (setf root z)
      (blacken z)
      (setf (parent z) nil)
      (return-from delete-node (values root t)))
    ;; link z where x was
    (if (left-child-p x)
        (setf (left (parent x)) z)
        (setf (right (parent x)) z))
    ;; next set parent of newly linked z to parent of x
    ;; that was where z is now. (but only if z is not sentinel)
    (unless (node-null z)
      (setf (parent z) (parent x)))
    ;; fixup
    (when (blackp x)
      (when (redp z)
        ;; When the deleted node x is black and its only child
        ;; and replacement z is red, blackening z respects the
        ;; black-depth of the tree.
        ;; In this case the black-depth from x is two and
        ;; therefore the black children of z must be leaf nodes.
        (blacken z)
        (return-from delete-node (values root t)))
      ;; In this case z is black and must be *sentinel*.
      ;; This is because the black-depth from x to its sentinel child is 2.
      ;; Now the other child z, if it is black, must be a leaf node.
      ;; (assert (node-null z))
      (setf root (fix-delete z (parent x) root))
      (return-from delete-node (values root t)))
    ;; When x is red then both its childs must be leaf nodes
    ;; because one child, namely z's sibling, is a leaf.
    ;; In this case x gets replaced by the leaf z and the
    ;; black-depth remains the same - no correction needs to be done.
    (values root t)))

;; piece-table specific

(declaim (ftype (function (idx node) (values node idx)) offset-to-node))
(defun offset-to-node (offset root)
  (declare #.*max-optimize-settings*)
  "Returns the node corresponding to OFFSET in ROOT's subtree. O(log n) number of nodes.
Note that the node is chosen such that its piece contains the character referred to by
OFFSET. Returns +sentinel+ if not found."
  (labels ((recur (offset node node-offset)
             (declare (type idx offset node-offset))
             (cond ((< offset (ltree-size node))
                    (recur offset (left node) node-offset))
                   ((< offset (+ (ltree-size node) (piece-size (piece node))))
                    (values node (+ node-offset (ltree-size node))))
                   (t
                    (recur
                     (- offset (+ (ltree-size node) (piece-size (piece node))))
                     (right node)
                     (+ node-offset (+ (ltree-size node) (piece-size (piece node)))))))))
    (declare (dynamic-extent #'recur))
    (recur offset root 0)))

(declaim (ftype (function (idx node) (values node idx idx)) lf-to-node))
(defun lf-to-node (n root)
  (declare #.*max-optimize-settings*)
  "Returns the node containing the Nth line in ROOT's subtree, the number of linefeeds
encountered up to that point and the node's absolute offset as multiple values."
  (labels ((recur (n node lf-count offset)
             (declare (type idx n lf-count offset))
             (cond ((< n (ltree-lfs node))
                    (recur n (left node) lf-count offset))
                   ((<= n (+ (ltree-lfs node) (piece-lf-count (piece node)))) ; 1-indexed
                    (values node (+ lf-count (ltree-lfs node))
                            (+ offset (ltree-size node))))
                   (t
                    (recur
                     (- n (+ (ltree-lfs node) (piece-lf-count (piece node))))
                     (right node)
                     (+ lf-count (+ (ltree-lfs node) (piece-lf-count (piece node))))
                     (+ offset (+ (ltree-size node) (piece-size (piece node)))))))))
    (declare (dynamic-extent #'recur))
    (recur n root 0 0)))

(defun node-to-offset (node root)
  (declare #.*max-optimize-settings*
           (type node node root))
  "Computes the absolute offset of NODE. O(log n). Only useful for debugging."
  (loop :until (eq node root)
        :with offset of-type idx = (ltree-size node)
        :finally (return offset)
        :do (when (right-child-p node)
              (incf offset (+ (piece-size (piece (parent node)))
                              (ltree-size (parent node)))))
            (setf node (parent node))))

;;;
;;; piece-table
;;;

(defstruct (piece-table (:conc-name pt-)
                        (:constructor %make-piece-table))
  "SIZE tracks the current size of the document in characters.
LINE-COUNT tracks the number of lines in the document.
INITIAL-BUFFER is a string holding the immutable original content of the document.
CHANGE-BUFFER is a string holding any inserted text.
ROOT is the root of the binary (red-black) tree containing PIECE descriptors.
CACHE is the most recently inserted node - used for optimization of (common) sequential
inserts."
  (size (required-arg 'size) :type idx)
  (line-count (required-arg 'line-count) :type idx)
  (initial-buffer (make-string 0) :type (simple-array character) :read-only t)
  (change-buffer (make-string 0) :type (simple-array character))
  (change-buffer-fill 0 :type idx)
  (root (required-arg 'root))
  (cache +sentinel+)
  (cache2 +sentinel+)
  (cache-offset2 0 :type idx)
  (cache-size2 0 :type idx))

(defun make-piece-table (&key (initial-contents ""))
  (let* ((length (length initial-contents))
         (linefeeds (count #\Newline initial-contents))
         (root (make-node (make-piece :offset 0
                                      :size length
                                      :lf-count linefeeds
                                      :buffer +initial-buffer+)
                          :color +black+)))
    (%make-piece-table :size length
                       :line-count (1+ linefeeds)
                       :initial-buffer (make-array length
                                                   :initial-contents initial-contents
                                                   :element-type 'character)
                       :root root
                       :cache2 root
                       :cache-size2 length)))

(defun pt-append-to-change-buffer (piece-table string string-length)
  (declare #.*max-optimize-settings*
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type string string)
           (type idx string-length))
  (let* ((old-fill (pt-change-buffer-fill piece-table))
         (old-actual-length (length (pt-change-buffer piece-table)))
         (new-fill (+ old-fill string-length)))
    (when (>= new-fill #.*max-buffer-size*)
      (error "buffer exceeded maximum size ~d!" #.*max-buffer-size*))
    (when (> new-fill old-actual-length)
      (setf (pt-change-buffer piece-table)
            (adjust-array (pt-change-buffer piece-table)
                          (max new-fill
                               (min (* old-actual-length 2) #.*max-buffer-size*)))))
    (replace (pt-change-buffer piece-table) string :start1 old-fill)
    (incf (pt-change-buffer-fill piece-table) string-length)))

(declaim (inline pt-piece-buffer
                 pt-fix-ltree-data
                 pt-insert-before pt-insert-after
                 pt-delete-node
                 pt-offset-to-node pt-lf-to-node pt-node-to-offset
                 pt-first-node pt-last-node
                 pt-get-piece-data pt-copy-piece-data
                 pt-count-linefeeds))

(declaim (ftype (function (piece-table piece) (simple-array character))
                pt-piece-buffer))
(defun pt-piece-buffer (piece-table piece)
  "Returns the data of the text-buffer associated with PIECE."
  (ecase (piece-buffer piece)
    (#.+change-buffer+ (pt-change-buffer piece-table))
    (#.+initial-buffer+ (pt-initial-buffer piece-table))))

(defun pt-fix-ltree-data (piece-table x)
  (fix-ltree-data x (pt-root piece-table)))

(defun pt-insert-before (piece-table piece x)
  (multiple-value-bind (new-root new)
      (insert-before piece x (pt-root piece-table))
    (setf (pt-root piece-table) new-root)
    new))

(defun pt-insert-after (piece-table piece x)
  (multiple-value-bind (new-root new)
      (insert-after piece x (pt-root piece-table))
    (setf (pt-root piece-table) new-root)
    new))

(defun pt-delete-node (piece-table y)
  (setf (pt-root piece-table) (delete-node y (pt-root piece-table)))
  y)

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
        (setf (pt-cache-size2 piece-table) (piece-size (piece node)))
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

(declaim (ftype (function (piece-table piece idx idx) (vector character))
                pt-get-piece-data))
(defun pt-get-piece-data (piece-table piece start-offset end-offset)
  "Do not modify return value. We return a displaced array to avoid copying huge pieces
of data."
  (make-array (- end-offset start-offset)
              :displaced-to (pt-piece-buffer piece-table piece)
              :displaced-index-offset (+ (piece-offset piece) start-offset)
              :element-type 'character))

(declaim (ftype (function (piece-table piece idx idx) (simple-array character))
                pt-copy-piece-data))
(defun pt-copy-piece-data (piece-table piece start-offset end-offset)
  (subseq (pt-piece-buffer piece-table piece)
          (+ (piece-offset piece) start-offset)
          (+ (piece-offset piece) end-offset)))

(declaim (ftype (function (piece-table piece idx idx) idx)
                pt-count-linefeeds))
(defun pt-count-linefeeds (piece-table piece start-offset end-offset)
  (count #\Newline (pt-piece-buffer piece-table piece)
         :start (+ (piece-offset piece) start-offset)
         :end (+ (piece-offset piece) end-offset)))

;;;
;;; interface
;;;

(define-condition piece-table-bounds-error (error)
  ((piece-table :initarg :piece-table
                :reader pt-bounds-error-piece-table
                :type piece-table)
   (bounds :initarg :bounds
           :reader pt-bounds-error-bounds
           :type (cons idx idx)))
  (:documentation "Signaled when trying to access out of bounds."))

(define-condition piece-table-bad-offset (piece-table-bounds-error)
  ((offset :initarg :offset
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

(defun pt-char (piece-table n)
  (declare #.*max-optimize-settings*
           (type idx n))
  (when (or (< n 0) (>= n (pt-size piece-table)))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset n
                                   :bounds (cons 0 (1- (pt-size piece-table)))))
  ;; find the corresponding node in O(log n); fetch its data
  (multiple-value-bind (node node-offset)
      (pt-offset-to-node piece-table n)
    (schar (pt-piece-buffer piece-table (piece node))
           (+ (piece-offset (piece node)) (- n node-offset)))))

(defun pt-subseq (piece-table start &optional (end (pt-size piece-table)))
  (declare #.*max-optimize-settings*
           (type idx start end))
  (when (> end (pt-size piece-table))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset end
                                   :bounds (cons 0 (pt-size piece-table))))
  (when (> start end)
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset start
                                   :bounds (cons 0 end)))
  (when (= start end)
    (return-from pt-subseq ""))
  ;; only search for START's node first - we can check whether END refers to the same
  ;; piece without searching vvvv
  (multiple-value-bind (start-node start-node-offset)
      (pt-offset-to-node piece-table start)
    (let ((start-piece (piece start-node)))
      ;; (hopefully) common case when start and end are spanned by 1 piece
      (when (<= end (+ start-node-offset (piece-size start-piece)))
        (return-from pt-subseq
          (pt-copy-piece-data piece-table start-piece
                              (- start start-node-offset)
                              (- end start-node-offset))))
      ;; we build up the string in piece-sized chunks by traversing between
      ;; start-node and end-node, concatenating their pieces' text to str
      (multiple-value-bind (end-node end-node-offset)
          (pt-offset-to-node piece-table (1- end)) ; we don't need end's character
        (let ((subseq (make-array (- end start) :element-type 'character)))
          (loop
            :initially (replace subseq (pt-piece-buffer piece-table start-piece)
                                :start2 start-piece-start-offset
                                :end2 start-piece-end-offset)
            :with start-piece-start-offset = (+ (piece-offset start-piece)
                                                (- start start-node-offset))
            :with start-piece-end-offset = (+ (piece-offset start-piece)
                                              (piece-size start-piece))
            :with subseq-start1
              of-type idx = (- start-piece-end-offset start-piece-start-offset)
            :with end-piece = (piece end-node)
            :for node = (next-node start-node) :then (next-node node)
            :until (eq node end-node)
            :with piece ; = (piece node) non-conforming. see ...CLHS/Body/06_aba.htm
            :do (setf piece (piece node))
                (replace subseq (pt-piece-buffer piece-table piece)
                         :start1 subseq-start1
                         :start2 (piece-offset piece)
                         :end2 (+ (piece-offset piece) (piece-size piece)))
                (incf subseq-start1 (piece-size piece))
            :finally (replace subseq (pt-piece-buffer piece-table end-piece)
                              :start1 subseq-start1
                              :start2 (piece-offset end-piece)
                              :end2 (+ (piece-offset end-piece)
                                       (- end end-node-offset))))
          subseq)))))

(defun pt-line-number-offset (piece-table line-number)
  (declare #.*max-optimize-settings*
           (type idx line-number))
  (let ((line-count (pt-line-count piece-table)))
    (when (or (<= line-number 0) (> line-number (1+ line-count)))
      (error 'piece-table-bad-line-number :piece-table piece-table
                                          :line-number line-number
                                          :bounds (cons 1 (1+ line-count))))
    (when (= line-number (1+ line-count))
      (return-from pt-line-number-offset (pt-size piece-table))))
  ;; I don't really feel a sentinel linebreak is aesthetically great. Neither is the
  ;; alternative of having lf-to-node return the offset of the first linebreak.
  (when (= line-number 1)
    (return-from pt-line-number-offset 0))
  ;; find the node containing the linebreak, search its data - linear, but very fast
  (multiple-value-bind (node lf-count offset) ; lf-count is at least 1
      (pt-lf-to-node piece-table (1- line-number))
    (loop
      :with piece = (piece node)
      :with text = (pt-piece-buffer piece-table piece)
      :with piece-offset = (piece-offset piece)
      :with end = (+ piece-offset (piece-size piece))
      :for start of-type idx = piece-offset then (1+ start)
      :until (= start end) ; loop until start = (1- end)
      :do (when (and (char= (schar text start) #\Newline)
                     (= (incf (the idx lf-count)) (1- line-number)))
            (return (1+ (+ offset (- start piece-offset))))))))

(defun pt-offset-in-bytes (piece-table offset)
  (declare #.*max-optimize-settings*
           (type idx offset))
  (when (or (< offset 0) (>= offset (pt-size piece-table)))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset offset
                                   :bounds (cons 0 (1- (pt-size piece-table)))))
  ;; TODO not fast enough. uncommon operation though...
  (when (= offset 0) (return-from pt-offset-in-bytes 0))
  (multiple-value-bind (end-node end-offset)
      (pt-offset-to-node piece-table (1- offset))
    (loop
      :with bytes of-type idx
      :for node = (pt-first-node piece-table) then (next-node node)
      :until (eq node end-node)
      :do (incf bytes (the idx (babel:string-size-in-octets
                                (pt-get-piece-data piece-table (piece node)
                                                   0
                                                   (piece-size (piece node))))))
      :finally (return (+ bytes (the idx (babel:string-size-in-octets
                                          (pt-get-piece-data piece-table (piece end-node)
                                                             0
                                                             (- offset end-offset)))))))))

(defun pt-insert (piece-table string offset)
  (declare #.*max-optimize-settings*
           (type string string)
           (type idx offset))
  (when (> offset (pt-size piece-table))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset offset
                                   :bounds (cons 0 (pt-size piece-table))))
  (let ((change-buffer-size (pt-change-buffer-fill piece-table)) ; store before inserting
        (length (length string))
        (lf-count (count #\Newline string)))
    (unless (= length 0) ; empty table case
      (pt-append-to-change-buffer piece-table string length)
      (incf (pt-size piece-table) length) ; be quiet. wtf sbcl?
      (incf (pt-line-count piece-table) lf-count)
      (cond ((= (pt-size piece-table) 0)
             (let* ((new-piece (make-piece :buffer +change-buffer+
                                           :offset change-buffer-size
                                           :size length
                                           :lf-count lf-count))
                    (new-node (make-node new-piece :color +black+)))
               (setf (pt-cache piece-table) new-node)
               (setf (pt-root piece-table) new-node)))
            ;; table is nonempty, insertion before start - cannot be cached
            ((= offset 0)
             (setf (pt-cache piece-table)
                   (pt-insert-before piece-table
                                     (make-piece :buffer +change-buffer+
                                                 :offset change-buffer-size
                                                 :size length
                                                 :lf-count lf-count)
                                     (pt-first-node piece-table))))
            (t
             (multiple-value-bind (node node-offset)
                 (pt-offset-to-node piece-table (1- offset)) ;we can ignore 0 and insert at
               (let* ((node-piece (piece node)) ;the right boundaries of pieces
                      (boundary (= offset (+ node-offset (piece-size node-piece)))))
                 ;; common case - appending to (cached) node corresponding to the
                 ;; end of the 'change' piece-table
                 (if (and (eq node (pt-cache piece-table)) boundary)
                     (progn
                       (incf (piece-size node-piece) length)
                       (incf (pt-cache-size2 piece-table) length)
                       (incf (piece-lf-count node-piece) lf-count)
                       (pt-fix-ltree-data piece-table node))
                     (let ((new (make-piece :buffer +change-buffer+
                                            :offset change-buffer-size
                                            :size length
                                            :lf-count lf-count)))
                       (if boundary ; insertion just after piece boundary
                           (setf (pt-cache piece-table)
                                 (pt-insert-after piece-table new node))
                           (let* ((right-size
                                    (- (+ node-offset (piece-size node-piece)) offset))
                                  (right-lfs
                                    (pt-count-linefeeds piece-table node-piece
                                                        (- offset node-offset)
                                                        (+ (- offset node-offset)
                                                           right-size)))
                                  (new-right
                                    (make-piece :buffer (piece-buffer node-piece)
                                                :offset (+ (piece-offset node-piece)
                                                           (- offset node-offset))
                                                :size right-size
                                                :lf-count right-lfs)))
                             (decf (pt-cache-size2 piece-table)
                                   (- (+ node-offset (piece-size node-piece)) offset))
                             (pt-insert-after piece-table new-right node)
                             (setf (pt-cache piece-table)
                                   (pt-insert-after piece-table new node))
                             (decf (piece-size node-piece) right-size)
                             (decf (piece-lf-count node-piece) right-lfs)
                             (pt-fix-ltree-data piece-table node))))))))))
    string)) ;corresponds to outermost let

(defun pt-erase-within-piece (piece-table node node-offset start end)
  (declare #.*max-optimize-settings*
           (type idx start end node-offset))
  (let* ((delta (- end start))
         (piece (piece node))
         (start-on-boundary (= start node-offset))
         (end-on-boundary (= end (+ node-offset (piece-size piece)))))
    (cond ((and start-on-boundary end-on-boundary)
           (let ((delete (pt-copy-piece-data piece-table piece
                                             0
                                             (piece-size piece))))
             (decf (pt-line-count piece-table) (piece-lf-count piece))
             (pt-delete-node piece-table node)
             delete))
          (start-on-boundary
           (let* ((delete (pt-copy-piece-data piece-table piece
                                              0
                                              (- end node-offset)))
                  (lf-delta (pt-count-linefeeds piece-table piece
                                                (- end node-offset)
                                                (piece-size piece))))
             (decf (pt-line-count piece-table) lf-delta)
             (incf (piece-offset piece) delta)
             (decf (piece-size piece) delta)
             (decf (piece-lf-count piece) lf-delta)
             (pt-fix-ltree-data piece-table node)
             delete))
          (end-on-boundary
           (let* ((delete (pt-copy-piece-data piece-table piece
                                              (- start node-offset)
                                              (piece-size piece)))
                  (lf-delta (pt-count-linefeeds piece-table piece
                                                0
                                                (- start node-offset))))
             (decf (pt-line-count piece-table) lf-delta)
             (setf (piece-size piece) (- start node-offset))
             (decf (piece-lf-count piece) lf-delta)
             (pt-fix-ltree-data piece-table node)
             delete))
          (t
           (let* ((delete (pt-copy-piece-data piece-table piece
                                              (- start node-offset)
                                              (- end node-offset)))
                  (before-lf (pt-count-linefeeds piece-table piece
                                                 0
                                                 (- start node-offset)))
                  (after-lf (pt-count-linefeeds piece-table piece
                                                (- end node-offset)
                                                (piece-size piece))))
             (decf (pt-line-count piece-table) (- (piece-lf-count piece)
                                                  (+ before-lf after-lf)))
             (pt-insert-after piece-table
                              (make-piece :buffer (piece-buffer piece)
                                          :offset (+ (piece-offset piece)
                                                     (- end node-offset))
                                          :size (- (+ node-offset (piece-size piece))
                                                   end)
                                          :lf-count after-lf)
                              node)
             (setf (piece-size piece) (- start node-offset))
             (setf (piece-lf-count piece) before-lf)
             (pt-fix-ltree-data piece-table node)
             delete)))))

(defun pt-erase-multiple (piece-table start-node start-node-offset start end)
  (declare #.*max-optimize-settings*
           (type idx start end start-node-offset))
  (let ((start-piece (piece start-node)))
    (multiple-value-bind (end-node end-node-offset)
        (pt-offset-to-node piece-table (1- end))
      (decf (pt-cache-offset2 piece-table) (- end end-node-offset))
      (decf (pt-cache-size2 piece-table) (- end end-node-offset))
      (loop
        :initially (replace return-buffer (pt-piece-buffer piece-table start-piece)
                            :start2 start-piece-start-offset
                            :end2 start-piece-end-offset)
                   (when (eq (next-node start-node) end-node)
                     (loop-finish))
        :with return-buffer = (make-array (- end start) :element-type 'character)
        :with start-piece-start-offset = (+ (piece-offset start-piece)
                                            (- start start-node-offset))
        :with start-piece-end-offset = (+ (piece-offset start-piece)
                                          (piece-size start-piece))
        :with ret-buffer-start1 of-type idx = (- start-piece-end-offset
                                                 start-piece-start-offset)
        :with end-piece = (piece end-node)
        :for delete = (next-node start-node)
        :for delete-piece = (piece delete)
        :until (eq delete-piece end-piece) ; could have been swapped back during
                                           ;; deletions. thus we proceed backwards from end-node for simplicity
        :do (replace return-buffer (pt-piece-buffer piece-table delete-piece)
                     :start1 ret-buffer-start1
                     :start2 (piece-offset delete-piece)
                     :end2 (+ (piece-offset delete-piece)
                              (piece-size delete-piece)))
            (incf ret-buffer-start1 (piece-size delete-piece))
            (decf (pt-line-count piece-table) (piece-lf-count delete-piece))
            (pt-delete-node piece-table delete)
        :finally (replace return-buffer (pt-piece-buffer piece-table end-piece)
                          :start1 ret-buffer-start1
                          :start2 (piece-offset end-piece)
                          :end2 (+ (piece-offset end-piece) (- end end-node-offset)))
                 (if (= end (+ end-node-offset (piece-size end-piece)))
                     (progn
                       (decf (pt-line-count piece-table) (piece-lf-count end-piece))
                       (pt-delete-node piece-table end-node))
                     (let ((lf-delta (pt-count-linefeeds piece-table end-piece
                                                         0
                                                         (- end end-node-offset))))
                       (decf (pt-line-count piece-table) lf-delta)
                       (decf (piece-size end-piece) (- end end-node-offset))
                       (incf (piece-offset end-piece) (- end end-node-offset))
                       (decf (piece-lf-count end-piece) lf-delta)
                       (pt-fix-ltree-data piece-table end-node)))
                 ;; end first - deletion can affect relative node position
                 (if (= start start-node-offset)
                     (progn
                       (decf (pt-line-count piece-table) (piece-lf-count end-piece))
                       (pt-delete-node piece-table start-node))
                     (let ((lf-delta (pt-count-linefeeds piece-table start-piece
                                                         (- start start-node-offset)
                                                         (piece-size start-piece))))
                       (setf (piece-size start-piece) (- start start-node-offset))
                       (decf (piece-lf-count start-piece) lf-delta)
                       (pt-fix-ltree-data piece-table start-node)))
                 (return return-buffer)))))

(defun pt-erase (piece-table start &optional (end (1+ start)))
  (declare #.*max-optimize-settings*
           (type idx start end))
  (when (< end start)
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset start
                                   :bounds (cons 0 end)))
  (when (> end (pt-size piece-table))
    (error 'piece-table-bad-offset :piece-table piece-table
                                   :offset end
                                   :bounds (cons 0 (pt-size piece-table))))
  (decf (pt-size piece-table) (- end start))
  (if (= start end)
      ""
      (multiple-value-bind (start-node start-node-offset)
          (pt-offset-to-node piece-table start)
        ;; if size becomes 0, cache is invalid and unreachable - we don't mind
        (decf (pt-cache-size2 piece-table)
              (- (+ (piece-size (piece start-node)) start-node-offset) start))
        ;; (hopefully) common case when start and end are spanned by 1 piece
        (if (<= end (+ start-node-offset (piece-size (piece start-node))))
            (pt-erase-within-piece piece-table start-node start-node-offset start end)
            (pt-erase-multiple piece-table start-node start-node-offset start end)))))

;;;
;;; Debugging & testing
;;;

(defun tree-to-list (root)
  "Returns the binary tree rooted at ROOT in list form."
  (cond ((node-null root) nil)
        (t
         (list root
               (tree-to-list (left root))
               (tree-to-list (right root))))))

(defun pt-tree (piece-table)
  (tree-to-list (pt-root piece-table)))

(defun pt-contents (piece-table)
  (pt-subseq piece-table 0))

(defun check-rbt (root)
  "Checks the red-black-tree conditions for the tree/subtree with root ROOT.
Returns the black depth of the tree on success.
TODO put in red-black tree tests"
  (labels ((recur (node)
             (cond ((node-null node) 1)
                   (t (let ((a (recur (left node)))
                            (b (recur (right node)))
                            (parcheck t))
                        (unless (node-null (left node))
                          (setf parcheck (eq (parent (left node)) node)))
                        (unless (node-null (right node))
                          ;; if parcheck is already wrong it's wrong
                          (setf parcheck (and parcheck (eq (parent (right node)) node))))
                        (if (or (not parcheck) (null a) (null b) (/= a b))
                            nil
                            (cond ((blackp node) (1+ a))
                                  (t
                                   (if (and (blackp (left node))
                                            (blackp (right node)))
                                       a
                                       nil)))))))))
    (and (blackp root) (null (parent root)) (recur root))))

(defun check-ltree (root)
  "Checks that the metadata stored in nodes is consistent in the tree with root ROOT.
Returns t upon success, nil otherwise.
TODO put in piece-table tests"
  (loop :for x = (leftmost root) :then (next-node x)
        :until (null x)
        :do (unless (and (= (calculate-size (left x)) (ltree-size x)))
              (format t "inconsistent node: size: ~d ltree: ~d~&"
                      (calculate-size (left x)) (ltree-size x))
              (return-from check-ltree (values nil x)))
            (unless (and (= (calculate-lfs (left x)) (ltree-lfs x)))
              (format t "inconsistent node: lfs: ~d ltree: ~d~&"
                      (calculate-lfs (left x)) (ltree-lfs x))
              (return-from check-ltree (values nil x))))
  t)

(defun rb-test (root n)
  (let ((nodes 1)
        (root root))
    (print :insert)
    (loop :repeat n
          :for node = (leftmost root)
          :do (dotimes (i (random nodes))
                (setf node (next-node node)))
              (setf root (insert-after (make-piece) node root))
              (assert (check-ltree root))
              (assert (check-rbt root))
              (incf nodes))
    (print :delete)
    (loop :repeat n
          :for node = (leftmost root)
          :do (dotimes (i (random nodes))
                (setf node (next-node node)))
              (setf root (delete-node node root))
              (assert (check-ltree root))
              (assert (check-rbt root))
              (decf nodes))
    root))

(defun pt-test (piece-table n length)
  "Insert N random strings of LENGTH into PIECE-TABLE."
  (let (words)
    (format t "generating words...~&")
    (dotimes (i n)
      (let ((str (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
        (dotimes (j length)
          (vector-push-extend ;(code-char (+ 161 (random 1000))) str
           (code-char (+ 33 (random 94))) str))
        (push str words)))
    (format t "inserting words...~&")
    (time
     (dotimes (i n)
       (pt-insert piece-table "this" (random (pt-size piece-table)))))))

;; delete-node tests

;; cond branch 1

;;                               +2+
;;                              /-  \-
;;                            /-      \-
;;                          /-          \-
;;                        /-              \-
;;                      /-                  \-
;;                    /-                      \-
;;                  /-                          \-
;;                +1+   <-- to be deleted (x,y)  +3+
;;               /-  \-
;;             /-      \-
;;           /-          \-
;;         /-              \-
;;       /-                  \-
;;     /-                      \-
;; +sentinel+               +sentinel+ -> z

(let* ((+1+ (%make-node :left +sentinel+
                        :right +sentinel+
                        :color +red+
                        :piece (make-piece :size 1)
                        :ltree-size 0))
       (+2+ (%make-node :left +1+
                        :color +black+
                        :piece (make-piece :size 2)
                        :ltree-size 1))
       (+3+ (%make-node :parent +2+
                        :left +sentinel+
                        :right +sentinel+
                        :color +red+
                        :piece (make-piece :size 3)
                        :ltree-size 0)))
  (setf (parent +1+) +2+)
  (setf (right +2+) +3+)
  (delete-node +1+ +2+)
  (assert (check-rbt +2+))
  (assert (check-ltree +2+)))

;; cond branch 2

;;TODO

(let* ()
  )

;; cond branch t

;;                              +5+
;;                             /-  \-
;;                           /-      \-
;;                         /-          \-
;;                       /-              \-
;;                     /-                  \-
;;                   /-                      \-
;;                 /-                         +6+
;;               +2+   <-- to be deleted (y)
;;              /-  \-
;;            /-      \-
;;          /-          \-
;;        /-              \-
;;      /-                  \-
;;    /-                      \-
;;  +1+                        +4+
;;                           -/
;;                         -/
;;                       -/
;;                     -/
;;                   -/
;;                 -/
;;               +3+ -> x, +sentinel+ -> z

(let* ((+1+ (%make-node :left +sentinel+
                        :right +sentinel+
                        :color +black+
                        :piece (make-piece :size 1)
                        :ltree-size 0))
       (+2+ (%make-node :left +1+
                        :right nil
                        :color +red+
                        :piece (make-piece :size 2)
                        :ltree-size 1))
       (+3+ (%make-node :left +sentinel+
                        :right +sentinel+
                        :color +red+
                        :piece (make-piece :size 3)
                        :ltree-size 0))
       (+4+ (%make-node :left +3+
                        :right +sentinel+
                        :parent +2+
                        :color +black+
                        :piece (make-piece :size 4)
                        :ltree-size 3))
       (+5+ (%make-node :left +2+
                        :right nil
                        :color +black+
                        :piece (make-piece :size 5)
                        :ltree-size 10))
       (+6+ (%make-node :left +sentinel+
                        :right +sentinel+
                        :parent +5+
                        :color +black+
                        :piece (make-piece :size 6)
                        :ltree-size 0)))
  (setf (parent +1+) +2+)
  (setf (right +2+) +4+)
  (setf (parent +3+) +4+)
  (setf (parent +2+) +5+)
  (setf (right +5+) +6+)
  (delete-node +2+ +5+)
  (assert (check-rbt +5+))
  (assert (check-ltree +5+)))

;; empty insertion case

(let ((piece-table (make-piece-table)))
  (pt-insert piece-table "hi" 0)
  (assert (string= (pt-contents piece-table) "hi")))

(defparameter *piece-table*
  (make-piece-table :initial-contents (format nil "line 1~@
                                                   line 2~@
                                                   line 3~@
                                                   line 4")))

#+sbcl (require 'sb-sprof)

;;;
;;; class
;;;

(defclass piece-table-buffer (buffer:buffer)
  ((%piece-table-struct :type piece-table)))

(defmethod initialize-instance :after ((buffer piece-table-buffer) &key initial-contents)
  (setf (slot-value buffer '%piece-table-struct)
        (apply #'make-piece-table (when initial-contents
                                    (list :initial-contents initial-contents)))))

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
