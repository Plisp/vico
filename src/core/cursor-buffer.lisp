(defpackage :vico-core.buffer.cursor-buffer
  (:use :cl)
  (:local-nicknames (:buf :vico-core.buffer))
  (:export #:cursored-buffer-mixin
           #:cursors))
(in-package :vico-core.buffer.cursor-buffer)

;; buffer mixin that manages updating indexs

(defclass cursored-buffer-mixin ()
  ((cursors :initform (make-array 0 :fill-pointer t :adjustable t)
            :reader cursors)))

(defmethod buf:make-cursor :around ((buffer cursored-buffer-mixin) index)
  (declare (ignore index))
  (let ((cursor (call-next-method)))
    (vector-push-extend (cursors (buf:cursor-buffer cursor))
                        (tg:make-weak-pointer cursor))))

;;TODO test this garbage
(defmethod buf:insert :after ((buffer cursored-buffer-mixin) string index)
  (loop :with cursors = (cursors buffer)
        :with delta = (length string)
        :with idx = 0
        :do (when (>= idx (length cursors))
              (return))
            (let ((cursor (tg:weak-pointer-value (aref cursors idx))))
              (if (null cursor)
                  (progn
                    (decf (fill-pointer cursors))
                    (setf (aref cursors idx) (aref cursors (length cursors))))
                  (progn
                    (incf idx)
                    (when (< index (buf:index-at cursor))
                      (incf (buf:index-at cursor) delta)
                      (setf (buf:cursor-dirty-p cursor) t)))))))

(defmethod buf:erase :after ((buffer cursored-buffer-mixin) start &optional end)
  (loop :with cursors = (cursors buffer)
        :with idx = 0
        :do (when (>= idx (length cursors))
              (return))
            (let ((cursor (tg:weak-pointer-value (aref cursors idx))))
              (if (null cursor)
                  (progn
                    (decf (fill-pointer cursors))
                    (setf (aref cursors idx) (aref cursors (length cursors))))
                  (progn
                    (incf idx)
                    (when (< start (buf:index-at cursor))
                      (if (<= (buf:index-at cursor) end)
                          (setf (buf:index-at cursor) start)
                          (decf (buf:index-at cursor) (- end start)))
                      (setf (buf:cursor-dirty-p cursor) t)))))))
