;;;; definition of frontend interface

(in-package :vico-term)

(defclass tui ()
  ((windows :accessor windows
            :initform (list)
            :type list)
   (buffers :initarg :buffers
            :initform (list)
            :accessor buffers
            :type list)))

(defun list-windows ()
  (windows *i*))

(defun (setf list-windows) (new-value)
  (setf (windows *i*) new-value))

(defun list-buffers ()
  (buffers *i*))

(defun (setf list-buffers) (new-value)
  (setf (buffers *i*) new-value))

(defclass tui-window (window)
  ((%top-line :initform 1
              :accessor %top-line)
   (window-x :initarg :window-x
             :accessor window-x)
   (window-y :initarg :window-y
             :accessor window-y)
   (window-width :initarg :window-width
                 :accessor window-width)
   (window-height :initarg :window-height
                  :accessor window-height)
   (window-buffer :initarg :window-buffer
                  :accessor window-buffer))
  (:documentation "The only type of window"))

(defun make-window (window-x window-y window-width window-height &key window-buffer)
  (make-instance 'tui-window :window-x window-x :window-y window-y
                             :window-width window-width :window-height window-height
                             :window-buffer window-buffer))

(defmethod window-buffer ((window tui-window))
  (window-buffer window))

;; TODO finish implementing window protocol
;; TODO implement window abstraction with borders

;; TODO smarter redisplay computation using edit history
(defmethod redisplay-window ((window tui-window) &key force-p)
  (declare (ignore force-p))
  (loop :initially (ti:tputs 'ti:cursor-address (window-x window) (window-y window))
        :for line from (%top-line window)
        :for line-offset = (line-number-offset (window-buffer window) line)
          then next-line-offset
        :for next-line-offset = (line-number-offset (window-buffer window) (1+ line))
        :while next-line-offset
        :for text = (subseq (window-buffer window) line-offset next-line-offset)
        :do (write-string text)
            (ti:tputs 'ti:cursor-address (1- line) 0)
            (princ #\Return)))
