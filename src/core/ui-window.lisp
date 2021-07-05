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
     ;; ,@(when (and (listp name) (eq (first name) 'setf))
     ;;     (list `(:method :after (,@arglist)
     ;;              (redisplay (window-ui window)))))
     ,(list :documentation (or documentation "undocumented"))))

(defclass window ()
  ((ui :initarg :ui
       :reader window-ui
       :type ui))
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

;; TODO I don't like this. Come up with declarative way of spec
(define-window-protocol move-window (window &key x y))
(define-window-protocol resize-window (window &key width height)
  "Should resize other windows appropriately")

(define-window-protocol window-top-line (window))
(define-window-protocol scroll-window (window lines))

(define-window-protocol window-point (window))

(define-window-protocol window-point-column (window)
  "Refers to the column which the point will attempt to move to when moving by lines.")
(defgeneric (setf window-point-column) (new-value window))

(define-window-protocol window-char-width (window char))
(define-window-protocol window-string-width (window string))

(defun window-point-to-max-column (window)
  (check-type window window)
  (with-accessors ((point window-point))
      window
    (buf:cursor-bol point)
    (loop :with move-cols = (window-point-column window)
          :until (or (not (plusp move-cols))
                     (= (buf:index-at point) (buf:size (window-buffer window)))
                     (char= (buf:char-at point) #\newline))
          :do (decf move-cols (window-char-width window (buf:char-at point)))
              (buf:cursor-next-char point))))

;; style spans

(defclass style-span (buf:span)
  ((style :initarg :style
          :initform (error "STYLE initarg not provided")
          :accessor span-style
          :type hl:style)))

(defgeneric buffer-styles-for-window (buffer start end window)
  (:method :around ((buffer buf:buffer) start end window)
    (check-type buffer buf:buffer)
    (check-type start (or buf:cursor fixnum))
    (check-type end (or buf:cursor fixnum))
    (assert (eq (window-buffer window) buffer))
    (if (buf:cursorp start)
        (assert (eq buffer (buf:cursor-buffer start)))
        (setf start (buf:make-cursor buffer start)))
    (if (buf:cursorp end)
        (assert (eq buffer (buf:cursor-buffer end)))
        (setf end (buf:make-cursor buffer end)))
    (call-next-method))
  (:method append ((buffer buf:buffer) start end window)
    (list))
  (:method-combination append)
  (:documentation "Returns a list of style spans for BUFFER between cursors/indexes START
and END. WINDOW is provided as a context arugmnet"))

(defun styles-for-window (window start end)
  (buffer-styles-for-window (window-buffer window) start end window))
