(defpackage :vico-core.buffer.marked-buffer
  (:use :cl)
  (:local-nicknames (:buffer :vico-core.buffer))
  (:export
   :mark :mark-buffer :mark-offset
   :move-mark
   :marked-buffer-mixin :marks))
(in-package :vico-core.buffer.marked-buffer)

;; marks are for monitoring offsets into a buffer
;; subclass marked-buffer-mixin to support custom marks

(defclass mark ()
  ((buffer :initarg :buffer
           :initform (error "buffer is a required initarg")
           :reader mark-buffer
           :type marked-buffer-mixin)
   (offset :initarg :offset
           :initform (error "offset is a required initarg")
           :accessor mark-offset
           :type fixnum)))

(defun move-mark (mark &optional (delta 1))
  (incf (mark-offset mark) delta))

(defclass marked-buffer-mixin ()
  ((marks :initform (make-array 0 :element-type 'mark :fill-pointer t :adjustable t)
          :accessor marks
          :type vector)))

(defmethod initialize-instance :after ((mark mark) &key)
  (check-type (mark-buffer mark) marked-buffer-mixin "a marked-buffer-mixin")
  (vector-push-extend (marks (mark-buffer mark)) (tg:make-weak-pointer mark)))

;;TODO test this garbage
(defmethod buffer:insert :after ((buffer marked-buffer-mixin) string offset)
  (loop :with marks = (marks buffer)
        :with delta = (length string)
        :with idx = 0
        :do (when (>= idx (length marks))
              (return))
            (let ((mark (tg:weak-pointer-value (aref marks idx))))
              (if (null mark)
                  (progn
                    (decf (fill-pointer marks))
                    (setf (aref marks idx) (aref marks (length marks))))
                  (progn
                    (incf idx)
                    (when (< offset (mark-offset mark))
                      (incf (mark-offset mark) delta)))))))

(defmethod buffer:erase :after ((buffer marked-buffer-mixin) start &optional end)
  (loop :with marks = (marks buffer)
        :with idx = 0
        :do (when (>= idx (length marks))
              (return))
            (let ((mark (tg:weak-pointer-value (aref marks idx))))
              (if (null mark)
                  (progn
                    (decf (fill-pointer marks))
                    (setf (aref marks idx) (aref marks (length marks))))
                  (progn
                    (incf idx)
                    (when (< start (mark-offset mark))
                      (if (<= (mark-offset mark) end)
                          (setf (mark-offset mark) start)
                          (decf (mark-offset mark) (- end start)))))))))
