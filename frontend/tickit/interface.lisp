(in-package :vico-tickit)

(defconstant +tickit-window-popup+
  (cffi:foreign-enum-value 'tkt:tickitwindowflags :tickit-window-popup))

;;; state class

(defclass tickit-instance ()
  ((%tickit :initarg :%tickit
            :reader %tickit
            :documentation "")
   (windows :initarg :buffers
            :type list
            :documentation "")
   (buffers :initarg :buffers
            :type list
            :documentation "")))

;; do not allow modification of the list through rplaca/d and such

(defmethod core:windows ((instance tickit-instance))
  (copy-list (slot-value instance 'windows)))

(defmethod (setf core:windows) (new-value (instance tickit-instance))
  (setf (slot-value instance 'windows) new-value))

(defmethod core:buffers ((instance tickit-instance))
  (copy-list (slot-value instance 'buffers)))

(defmethod (setf core:buffers) (new-value (instance tickit-instance))
  (setf (slot-value instance 'buffers) new-value))

;;; timers

(defstruct timer
  callback)

;; (defmethod core:make-timer ((instance tickit-instance) function)
;;   (cffi:defcallback ))

;; (defmethod core:schedule-timer ((timer tickit-timer) delay &key repeat)
;;   ())

;; (defmethod core:unschedule-timer ((timer tickit-timer))
;;   (tkt:tickit-watch-cancel))

;;; window interface

(defclass tickit-window (core:window)
  ((%tickit-window :initarg :%tickit-window
                   :reader %tickit-window
                   :documentation "")
   (buffer :initarg :buffer
           :accessor buffer
           :documentation "")))

;; TODO need way of querying floating status?

(defmethod core:make-window ((instance tickit-instance) x y dx dy
                             &key buffer (floating nil))
  (cffi:with-foreign-object (rect '(:struct tkt:tickitrect))
    (setf (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:top) y
          (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:left) x
          (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:lines) dy
          (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:cols) dx)
    (let* ((flags (if floating +tickit-window-popup+ 0))
           (new (tkt:tickit-window-new-ptr
                 (tkt:tickit-get-rootwin (%tickit-window instance)) rect flags)))
      (push (make-instance 'tickit-window :%tickit-window new :buffer buffer)
            (core:buffers instance)))))

(defmethod core:window-buffer ((window tickit-window))
  (buffer window))

(defmethod core:window-x ((window tickit-window))
  (let ((rect (tkt:tickit-window-get-geometry-ptr (%tickit-window window))))
    (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:left)
    (cffi:foreign-free rect)))

(defmethod core:window-y ((window tickit-window))
  (let ((rect (tkt:tickit-window-get-geometry-ptr (%tickit-window window))))
    (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:top)
    (cffi:foreign-free rect)))

(defmethod core:window-width ((window tickit-window))
  (let ((rect (tkt:tickit-window-get-geometry-ptr (%tickit-window window))))
    (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:cols)
    (cffi:foreign-free rect)))

(defmethod core:window-height ((window tickit-window))
  (let ((rect (tkt:tickit-window-get-geometry-ptr (%tickit-window window))))
    (cffi:foreign-slot-value rect '(:struct tkt:tickitrect) 'tkt:lines)
    (cffi:foreign-free rect)))

(defmethod core:window-string-width ((window tickit-window) string)
  (loop :for c across string
        :summing (cond ((= (char-code c) 0) 2) ; NUL is ^@
                       ((= (char-code c) 9) 8) ; TODO tab character
                       (t
                        (let ((width (vico-tickit.util:wide-character-width c)))
                          (if (= width -1)
                              2 ; control characters are displayed as ^*
                              width))))))

(defmethod core:window-line-height ((window tickit-window))
  1)

(defmethod core:move-window ((window tickit-window) x y)
  (tkt:tickit-window-reposition (%tickit-window window) x y))

(defmethod core:resize-window ((window tickit-window) width height)
  (tkt:tickit-window-resize (%tickit-window window) height width))

(defmethod core:raise-window ((window tickit-window))
  (tkt:tickit-window-raise (%tickit-window window)))

(defmethod core:lower-window ((window tickit-window))
  (tkt:tickit-window-lower (%tickit-window window)))

;; TODO computation of changed region

(defmethod core:redisplay-window ((window tickit-window) &optional (force-p t))
  (if force-p
      (let ((rect (tkt:tickit-window-get-geometry-ptr (%tickit-window window))))
        (tkt:tickit-window-expose (%tickit-window window) rect))
      ()))
