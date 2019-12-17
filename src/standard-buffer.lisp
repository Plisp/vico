(defpackage :vico-lib.standard-buffer
  (:use :cl :vico-lib.evloop)
  (:local-nicknames (:core :vico-core))
  (:export #:standard-buffer
           #:keybinds))
(in-package :vico-lib.standard-buffer)

(defclass standard-buffer (core:piece-table-buffer)
  ((keybinds :initarg :local-keybinds
             :reader keybinds
             :type list))
  (:documentation "Standard buffer."))

(defmethod initialize-instance :after ((buffer standard-buffer)
                                       &key local-keybinds (inherit-keybinds t))
  (when inherit-keybinds
    (setf (slot-value buffer 'keybinds) (append local-keybinds
                                                vico-lib.key-event:*default-keybinds*))))
