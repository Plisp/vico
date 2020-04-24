(defpackage :vico-core.standard-buffer
  (:use :cl :vico-core.evloop)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:hl :vico-core.syntax-highlighting)
                    (:key :vico-core.key-event))
  (:export #:standard-buffer
           #:windows))
(in-package :vico-core.standard-buffer)

(defclass standard-buffer (vico-core.buffer.piece-table:piece-table-buffer
                           vico-core.buffer.cursor-buffer:cursored-buffer-mixin)
  ((keybinds :initarg :local-keybinds
             :accessor vico-core.buffer:keybinds
             :type list)
   (name :initarg :name
         :accessor vico-core.buffer:buffer-name)
   (lexers :initarg :lexers ;TODO determine via some 'file type' config variable
           :initform (list 'hl:cl-lexer 'hl:todo-lexer)
           :accessor lexers
           :documentation "ordered list"))
  (:documentation "Standard buffer."))

(defmethod initialize-instance :after ((buffer standard-buffer) &key local-keybinds
                                                                  (inherit-keybinds t)
                                                                  initial-file)
  (when initial-file
    (setf (buf:buffer-name buffer) initial-file))
  (when inherit-keybinds
    (setf (buf:keybinds buffer) (append local-keybinds key:*default-keybinds*))))
