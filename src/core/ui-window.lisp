(in-package :vico-core.ui)

(define-condition vico-window-unimplemented-error (vico-protocol-unimplemented-error)
  ((ui-type :initform 'window
            :reader ui-type)))

(defmacro define-window-protocol (name (&rest arglist) &optional documentation)
  `(defgeneric ,name (,@arglist)
     (:method (,@arglist)
       (declare (ignorable ,@(loop :for arg in arglist
                                   :unless (position arg lambda-list-keywords)
                                     :collect arg)))
       (error 'vico-window-unimplemented-error
              :function ',name
              :object ,(if (and (listp name) (eq (first name) 'setf))
                           (second arglist)
                           (first arglist))))
     ,@(when (and (listp name) (eq (first name) 'setf))
         (list `(:method :after (,@arglist)
                  (redisplay (window-ui window)))))
     ,(list :documentation (or documentation "undocumented"))))

(defclass window ()
  ((ui :initarg :ui
       :reader window-ui))
  (:documentation "All windows should subclass this protocol class"))

(define-window-protocol make-window (ui x y width height &key buffer &allow-other-keys)
  "Make a window at X, Y in 'native' coordinates extending WIDTH in the X direction and
HEIGHT in the Y direction, initially displaying the buffer BUFFER. Returns an opaque
window object usable with the other window interface functions. Frontends may accept
additional key parameters appropriate to their platform's capabilities.")

(define-window-protocol window-name (window))
(define-window-protocol (setf window-name) (new-value window))

(define-window-protocol window-buffer (window))
(define-window-protocol (setf window-buffer) (new-value window))

(define-window-protocol window-x (window))
(define-window-protocol (setf window-x) (new-value window))

(define-window-protocol window-y (window))
(define-window-protocol (setf window-y) (new-value window))

(define-window-protocol window-width (window))
(define-window-protocol (setf window-width) (new-value window))

(define-window-protocol window-height (window))
(define-window-protocol (setf window-height) (new-value window))

(define-window-protocol move-window (window &key x y))
(define-window-protocol resize-window (window &key width height)
  "should resize other windows appropriately")

(define-window-protocol window-top-line (window))
(define-window-protocol scroll-window (window lines))

(define-window-protocol window-point (window))
(define-window-protocol move-point (window &optional count))
(define-window-protocol move-point-lines (window &optional count))
