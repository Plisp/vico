(defpackage :vico-core.standard-buffer
  (:use :cl :vico-core.evloop)
  (:local-nicknames (:buffer :vico-core.buffer)
                    (:syn :vico-core.syntax-highlighting)
                    (:key :vico-core.key-event))
  (:export #:standard-buffer))
(in-package :vico-core.standard-buffer)

(defclass standard-buffer (vico-core.buffer.piece-table:piece-table-buffer)
  ((keybinds :initarg :local-keybinds
             :accessor vico-core.buffer:keybinds
             :type list)
   (name :initarg :name
         :accessor vico-core.buffer:buffer-name)
   (lexers :initarg :lexers ;TODO determine via some 'file type' config variable
           :initform (list 'syn:cl-lexer 'syn:todo-lexer)
           :accessor lexers
           :documentation "ordered list"))
  (:documentation "Standard buffer."))

(defmethod initialize-instance :after ((buffer standard-buffer) &key local-keybinds
                                                                  (inherit-keybinds t)
                                                                  initial-file)
  (when initial-file
    (setf (buffer:buffer-name buffer) initial-file))
  (when inherit-keybinds
    (setf (buffer:keybinds buffer) (append local-keybinds key:*default-keybinds*))))
