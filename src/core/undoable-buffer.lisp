(defpackage :vico-core.buffer.undoable-buffer
  (:use :cl)
  (:local-nicknames (:buffer :vico-core.buffer))
  (:export #:undoable-buffer-mixin
           #:with-grouped-edits))
(in-package :vico-core.buffer.undoable-buffer)

(defvar *edit-counter* 0)
(defvar *within-undo/redo-context* t)

(defclass undoable-buffer-mixin ()
  ((undo-stack :initform (list)
               :accessor undo-stack)
   (redo-stack :initform (list)
               :accessor redo-stack)))

(defun %undo (edit buffer)
  (declare (type undoable-buffer-mixin buffer))
  (let ((*within-undo/redo-context* nil)
        (edit-type (first edit))
        (string-or-reps (second edit))
        (offset (third edit)))
    (case edit-type
      (:insert (buffer:erase buffer offset (+ offset (length string-or-reps))))
      (:erase (buffer:insert buffer string-or-reps offset))
      (:group
       (dotimes (i string-or-reps (push (pop (undo-stack buffer)) (redo-stack buffer)))
         (buffer:undo buffer))))))

(defun %redo (edit buffer)
  (declare (type undoable-buffer-mixin buffer))
  (let ((*within-undo/redo-context* nil)
        (edit-type (first edit))
        (string-or-reps (second edit))
        (offset (third edit)))
    (case edit-type
      (:insert (buffer:insert buffer string-or-reps offset))
      (:erase (buffer:erase buffer offset (+ offset (length string-or-reps))))
      (:group
       (dotimes (i string-or-reps (push (pop (redo-stack buffer)) (undo-stack buffer)))
         (buffer:redo buffer))))))

(defmacro with-grouped-edits ((buffer) &body body)
  `(let ((*edit-counter* 0)
         (edit (list :group nil)))
     (push edit (undo-stack ,buffer))
     ,@body
     (push edit (undo-stack ,buffer))
     (setf (second edit) *edit-counter*)))

(defmethod buffer:insert :after ((buffer undoable-buffer-mixin) string offset)
  (when *within-undo/redo-context*
    (incf *edit-counter*)
    (setf (redo-stack buffer) (list))
    (push (list :insert string offset) (undo-stack buffer))))

(defmethod buffer:erase :around ((buffer undoable-buffer-mixin) start &optional end)
  (declare (ignore end))
  (when *within-undo/redo-context*
    (incf *edit-counter*)
    (let ((string (call-next-method)))
      (setf (redo-stack buffer) (list))
      (push (list :erase string start) (undo-stack buffer)))))

(defmethod buffer:undo ((buffer undoable-buffer-mixin)) ;TODO this is a user error
  (if (undo-stack buffer)
      (let ((edit (pop (undo-stack buffer))))
        (push edit (redo-stack buffer))
        (%undo edit buffer))
      (error "already at start of history")))

(defmethod buffer:redo ((buffer undoable-buffer-mixin))
  (if (redo-stack buffer)
      (let ((edit (pop (redo-stack buffer))))
        (push edit (undo-stack buffer))
        (%redo edit buffer))
      (error "already at end of history")))
