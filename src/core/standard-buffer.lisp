(defpackage :vico-core.standard-buffer
  (:use :cl :vico-core.evloop)
  (:local-nicknames (:buffer :vico-core.buffer)
                    (:key :vico-core.key-event))
  (:export #:standard-buffer))
(in-package :vico-core.standard-buffer)

(defclass standard-buffer (vico-core.buffer.piece-table:piece-table-buffer)
  ((keybinds :initarg :local-keybinds
             :reader vico-core.buffer:keybinds
             :type list))
  (:documentation "Standard buffer."))

(defmethod initialize-instance :after ((buffer standard-buffer)
                                       &key local-keybinds (inherit-keybinds t))
  (when inherit-keybinds
    (setf (slot-value buffer 'keybinds) (append local-keybinds key:*default-keybinds*))))
