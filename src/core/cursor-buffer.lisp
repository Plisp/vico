(defpackage :vico-core.buffer.cursor-buffer
  (:use :cl)
  (:local-nicknames (:buf :vico-core.buffer))
  (:export #:cursored-buffer-mixin
           #:cursors))
(in-package :vico-core.buffer.cursor-buffer)

;; buffer mixin that manages updating indexes

(defclass cursored-buffer-mixin ()
  ((cursors :initform (make-array 0 :fill-pointer t :adjustable t)
            :reader cursors)))

(defmethod buf:make-cursor :around ((buffer cursored-buffer-mixin) index)
  (declare (ignore index))
  (let ((cursor (call-next-method)))
    (vector-push-extend (tg:make-weak-pointer cursor) (cursors (buf:cursor-buffer cursor)))
    cursor))

;; (defmethod buf:insert :after ((buffer cursored-buffer-mixin) string index)
;;   (loop :with cursors = (cursors buffer)
;;         :with delta = (length string)
;;         :with idx = 0
;;         :do (when (>= idx (length cursors))
;;               (return))
;;             (let ((cursor (tg:weak-pointer-value (aref cursors idx))))
;;               (if (null cursor)
;;                   (progn
;;                     (decf (fill-pointer cursors))
;;                     (setf (aref cursors idx) (aref cursors (length cursors))))
;;                   (progn
;;                     (incf idx)
;;                     (when (< index (buf:index-at cursor))
;;                       (incf (buf:index-at cursor) delta)
;;                       (buf:dirty-cursor cursor)))))))

;; (defmethod buf:erase :after ((buffer cursored-buffer-mixin) start &optional end)
;;   (loop :with cursors = (cursors buffer)
;;         :with idx = 0
;;         :do (when (>= idx (length cursors))
;;               (return))
;;             (let ((cursor (tg:weak-pointer-value (aref cursors idx))))
;;               (if (null cursor)
;;                   (progn
;;                     (decf (fill-pointer cursors))
;;                     (setf (aref cursors idx) (aref cursors (length cursors))))
;;                   (progn
;;                     (incf idx)
;;                     (when (< start (buf:index-at cursor))
;;                       (if (<= (buf:index-at cursor) end)
;;                           (setf (buf:index-at cursor) start)
;;                           (decf (buf:index-at cursor) (- end start)))
;;                       (buf:dirty-cursor cursor)))))))

;; XXX far too slow, we need manual opt-in cursor tracking

;; (defmethod buf:insert-at :after (insert-cursor string)
;;   (let ((buffer (buf:cursor-buffer insert-cursor)))
;;     (when (typep buffer 'cursored-buffer-mixin)
;;       (loop :with cursors = (cursors buffer)
;;             :with delta = (length string)
;;             :with idx = 0
;;             :do (when (>= idx (length cursors))
;;                   (return))
;;                 (let ((cursor (tg:weak-pointer-value (aref cursors idx))))
;;                   (if (null cursor)
;;                       (progn
;;                         (decf (fill-pointer cursors))
;;                         (setf (aref cursors idx) (aref cursors (length cursors))))
;;                       (progn
;;                         (incf idx)
;;                         (when (< (buf:index-at insert-cursor) (buf:index-at cursor))
;;                           (incf (buf:index-at cursor) delta)
;;                           (buf:dirty-cursor cursor)))))))))

;; (defmethod buf:erase-at :after (erase-cursor &optional count)
;;   (let ((buffer (buf:cursor-buffer erase-cursor)))
;;     (when (typep buffer 'cursored-buffer-mixin)
;;       (loop :with cursors = (cursors buffer)
;;             :with start = (buf:index-at erase-cursor)
;;             :with end = (+ start count)
;;             :with idx = 0
;;             :do (when (>= idx (length cursors))
;;                   (return))
;;                 (let ((cursor (tg:weak-pointer-value (aref cursors idx))))
;;                   (if (null cursor)
;;                       (progn
;;                         (decf (fill-pointer cursors))
;;                         (setf (aref cursors idx) (aref cursors (length cursors))))
;;                       (progn
;;                         (incf idx)
;;                         (when (< start (buf:index-at cursor))
;;                           (if (<= (buf:index-at cursor) end)
;;                               (setf (buf:index-at cursor) start)
;;                               (decf (buf:index-at cursor) (- end start)))
;;                           (buf:dirty-cursor cursor)))))))))
