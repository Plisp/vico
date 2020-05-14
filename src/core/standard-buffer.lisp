(defpackage :vico-core.standard-buffer
  (:use :cl :vico-core.evloop)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:hl :vico-core.syntax-highlighting)
                    (:key :vico-core.key-event))
  (:export #:standard-buffer
           #:name
           #:keybinds
           #:lexers
           #:last-edit-time))
(in-package :vico-core.standard-buffer)

(defclass standard-buffer (vico-core.buffer.piece-table:piece-table-buffer
                           vico-core.buffer.cursor-buffer:cursored-buffer-mixin)
  ((keybinds :initarg :local-keybinds
             :accessor buf:keybinds
             :type list)
   (name :initarg :name
         :accessor buf:buffer-name)
   (lexers :initarg :lexers ;TODO determine via some 'file type' config variable
           :initform (list 'hl:cl-lexer 'hl:todo-lexer)
           :accessor lexers
           :documentation "ordered list")
   (edit-timestamp :initform 0
                   :accessor buf:edit-timestamp))
  (:documentation "Standard buffer."))

(defmethod initialize-instance :after ((buffer standard-buffer) &key local-keybinds
                                                                  (inherit-keybinds t)
                                                                  initial-file)
  (when initial-file
    (setf (buf:buffer-name buffer) initial-file))
  (when inherit-keybinds
    (setf (buf:keybinds buffer) (append local-keybinds key:*default-keybinds*))))

(defmethod buf:insert :after ((buffer standard-buffer) string index)
  (declare (ignore string index))
  (when (typep buffer 'standard-buffer)
    (incf (buf:edit-timestamp buffer))))

(defmethod buf:erase :after ((buffer standard-buffer) start &optional count)
  (declare (ignore start count))
  (when (typep buffer 'standard-buffer)
    (incf (buf:edit-timestamp buffer))))

(defmethod buf:insert-at :after (cursor string)
  (declare (ignore string))
  (let ((buffer (buf:cursor-buffer cursor)))
    (when (typep buffer 'standard-buffer)
      (incf (buf:edit-timestamp buffer)))))

(defmethod buf:erase-at :after (cursor &optional count)
  (declare (ignore count))
  (let ((buffer (buf:cursor-buffer cursor)))
    (when (typep buffer 'standard-buffer)
      (incf (buf:edit-timestamp buffer)))))
