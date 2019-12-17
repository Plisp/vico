(in-package :vico-lib.ui)

(defclass window ()
  ((ui :initarg :ui
       :reader window-ui))
  (:documentation "All windows should subclass this protocol class"))

(define-protocol make-window (ui x y width height &key buffer &allow-other-keys)
  "'Physically' construct a window at X, Y in 'native' coordinates extending WIDTH in the
X direction and HEIGHT in the Y direction, initially displaying the buffer BUFFER. Returns
an opaque window object usable with the other window interface functions. Frontends may
accept additional key parameters appropriate to their platform's capabilities.")

(define-protocol window-name (window))
(define-protocol (setf window-name) (new-value window))

(define-protocol window-buffer (window))
(define-protocol (setf window-buffer) (new-value window))

(define-protocol redisplay-window (window &key force-p)
  "Redisplays the window WINDOW in a thread-safe manner. If FORCE-P is t, redisplay the
whole window. Returns WINDOW on success.")

(define-protocol window-x (window))
(define-protocol (setf window-x) (new-value window))

(define-protocol window-y (window))
(define-protocol (setf window-y) (new-value window))

(define-protocol window-width (window))
(define-protocol (setf window-width) (new-value window))

(define-protocol window-height (window))
(define-protocol (setf window-height) (new-value window))

(define-protocol window-string-width (window string))
(define-protocol window-line-height (window))

(define-protocol move-window (window))
(define-protocol resize-window (window))

(define-protocol raise-window (window))
(define-protocol lower-window (window))

(define-protocol top-line (window))
(define-protocol (setf top-line) (new-value window))
