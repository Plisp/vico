(defpackage :vico-core.standard-buffer
  (:use :cl :vico-core.evloop)
  (:local-nicknames (:buffer :vico-core.buffer)
                    (:key :vico-core.key-event))
  (:export #:standard-buffer))
(in-package :vico-core.standard-buffer)

(defclass standard-buffer (vico-core.buffer.piece-table:piece-table-buffer)
  ((keybinds :initarg :local-keybinds
             :accessor vico-core.buffer:keybinds
             :type list)
   (name :initarg :name
         :accessor vico-core.buffer:buffer-name))
  (:documentation "Standard buffer."))

(defmethod initialize-instance :after ((buffer standard-buffer) &key local-keybinds
                                                                  (inherit-keybinds t)
                                                                  initial-file)
  (when initial-file
    (setf (buffer:buffer-name buffer) initial-file))
  (when inherit-keybinds
    (setf (buffer:keybinds buffer) (append local-keybinds key:*default-keybinds*))))
