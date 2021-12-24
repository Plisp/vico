(defpackage #:vico-sdl
  (:use :cl)
  (:import-from #:raw-bindings-sdl2 #:x #:y #:h #:w)
  (:local-nicknames (#:buf #:vico-core.buffer)
                    (#:ed #:vico-core.editor)
                    (#:win #:vico-core.window)
                    (#:cltl2 #:cl-environments)
                    (#:fonts #:org.shirakumo.font-discovery)
                    (#:sdl #:raw-bindings-sdl2)
                    (#:sdl-ttf #:raw-bindings-sdl2-ttf))
  (:export #:main))
(in-package #:vico-sdl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sdl-frontend (ed:frontend)
  ((font :initform '(:family "InputSans" :weight 90)
         :accessor font)
   %cwidth
   %cheight
   %ttf-font
   %renderer
   %window
   ))

(defclass sdl-window (win:window)
  ((top :initarg :top
        :accessor top
        :type buf:cursor)
   (point :initarg :point
          :accessor point
          :type buf:cursor
          :documentation "point is the editing cursor")
   (point-column ) ; TODO save column
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ui* nil)
(defvar *offset* 0)
;; TODO custom variables
(defparameter *font-size* 20)
(defparameter *empty-line-spacing* 8)
(defparameter *text-color* '(0 43 54 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thanks zulu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cltl2:define-declaration cffi-type (arg-var env-var)
  (declare (ignore env-var))
  (values
   :variable
   (destructuring-bind (cffi-type &rest variables) arg-var
     (mapcar (lambda (var) (list var 'cffi-type cffi-type)) ; (list var k v)
             variables))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cffi-type-or-error (var &optional env)
    "Returns the `cffi-type' of the variable `var' in `env', or signals an error."
    (or (multiple-value-bind (binding local-p decl)
            (cltl2:variable-information var env)
          (declare (ignore binding local-p))
          (cdr (assoc 'cffi-type decl)))
        (error "'~A' has no visible cffi type" var))))

(defmacro c-> (variable slot &environment env)
  `(cffi:foreign-slot-value ,variable ',(cffi-type-or-error variable env) ,slot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-windows (fn layout)
  (labels ((rec (layout)
             (if (consp (car layout))
                 (mapcar #'rec layout)
                 layout)))
    (mapcar fn (rec layout))))

(defun layout-windows ()
  (map-windows (lambda (layout)
                 (destructuring-bind (window (width . height) (x . y))
                     layout
                   (setf (win:w window) width
                         (win:h window) height
                         (win:x window) x
                         (win:y window) y)))
               (win::calc-layout
                (ed:layout *ui*)
                (cons (ed:width *ui*) (ed:height *ui*)))))

(defun measure-width (string)
  (cffi:with-foreign-object (%w :int)
    (with-slots (%ttf-font) *ui*
      (sdl-ttf:ttf-size-utf8 %ttf-font string %w (cffi:null-pointer))
      (cffi:mem-aref %w :int))))

(defun render-window (window)
  (with-slots (%renderer %ttf-font %cwidth %cheight) *ui*
    (buf:with-cursors* ((current (top window))
                        (next current))
      ;; XXX HOW DO YOU RENDER UNICODE? this will stop early for zero-width characters
      (loop :with last-y = (win:y window)
            :with linebuf = (make-array 80
                                        :element-type 'character
                                        :fill-pointer 0 :adjustable t)
            :with next-line? = (buf:cursor-next-line next)
            :for line-count :from 1 :upto (1+ (truncate (ed:height *ui*) %cheight))
            :while next-line?
            :do (buf:with-cursor (char-cursor current)
                  (with-output-to-string (line linebuf)
                    (loop :with line-idx = 0
                          :with limit = (min (truncate (ed:width *ui*) %cwidth)
                                             (- (buf:cursor- next current) 1))
                          :do (when (buf:cursor= char-cursor (point (first (ed:layout *ui*))))
                                (sdl:sdl-set-render-draw-color %renderer 42 161 152 20)
                                (cffi:with-foreign-object (rect 'sdl:sdl-rect)
                                  (cffi:with-foreign-slots ((x y w h) rect sdl:sdl-rect)
                                    (finish-output line)
                                    (setf x (1- (measure-width linebuf))
                                          y last-y
                                          w 3
                                          h %cheight))
                                  (sdl:sdl-render-fill-rect %renderer rect))
                                (apply #'sdl:sdl-set-render-draw-color %renderer *text-color*))
                          :while (< (buf:cursor- char-cursor current) limit)
                          :do ;; collecting chars in line
                              (let* ((char (or (buf:char-at char-cursor)
                                               (loop-finish)))
                                     (code (char-code char)))
                                (cond ((or (<= 0 code 31) (= code 127))
                                       (format line "^~c" (code-char (logxor #x40 code))))
                                      ((char= char #\tab)
                                       (format line "    "))
                                      (t
                                       (write-char char line))))
                              (buf:cursor-next-char char-cursor))))
                (if (= (length linebuf) 0)
                    (incf last-y *empty-line-spacing*)
                    (let* ((surface (sdl-ttf:ttf-render-utf8-blended
                                     %ttf-font linebuf '(sdl:r 147 sdl:g 161 sdl:b 161)))
                           (texture
                             (sdl:sdl-create-texture-from-surface %renderer surface)))
                      (cffi:with-foreign-object (rect 'sdl:sdl-rect)
                        (declare (cffi-type sdl:sdl-rect rect)
                                 (cffi-type sdl:sdl-surface surface))
                        ;; because with-foreign-slots is broken and doesn't support naming
                        (setf (c-> rect 'x) 0
                              (c-> rect 'y) last-y
                              (c-> rect 'w) (c-> surface 'w)
                              (c-> rect 'h) (c-> surface 'h))
                        ;; first lin may be clipped
                        (if (> line-count 1)
                            (sdl:sdl-render-copy %renderer texture (cffi:null-pointer) rect)
                            (cffi:with-foreign-object (clip 'sdl:sdl-rect)
                              (cffi:with-foreign-slots ((x y w h) clip sdl:sdl-rect)
                                (setf x 0
                                      y *offset*
                                      w (c-> surface 'w)
                                      h (- (c-> surface 'h) *offset*)))
                              (decf (c-> rect 'h) *offset*)
                              (sdl:sdl-render-copy %renderer texture clip rect)))
                        ;; green bounding rectangle
                        (sdl:sdl-set-render-draw-color %renderer 50 100 50 0)
                        (sdl:sdl-render-draw-rect %renderer rect)
                        (apply #'sdl:sdl-set-render-draw-color %renderer *text-color*)
                        ;;
                        (incf last-y (c-> rect 'h)))
                      (sdl:sdl-free-surface surface)
                      (sdl:sdl-destroy-texture texture)))
                (setf (fill-pointer linebuf) 0)
                ;;
                (buf:cursor-next-line current)
                (setf next-line? (buf:cursor-next-line next))
            ))))

(defun render ()
  (with-slots (%renderer) *ui*
    (sdl:sdl-render-clear %renderer)
    (layout-windows)
    (mapcar #'render-window
            (delete-if-not #'(lambda (w) (typep w 'sdl-window))
                           (alexandria:flatten (ed:layout *ui*))))
    (sdl:sdl-render-present %renderer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-event (event)
  (restart-case
      (case (cffi:foreign-slot-value event 'sdl:sdl-event 'sdl:type)
        ;; window close
        (#.sdl:+sdl-quit+
         (setf (ed:running? *ui*) nil))

        ;; scroll
        (#.sdl:+sdl-mousewheel+
         (case (cffi:foreign-slot-value event 'sdl:sdl-mouse-wheel-event 'y)
           (1 ;(incf *offset* 5)
            (buf:cursor-next-line (top (first (ed:layout *ui*))))
            )
           (-1 ;(setf *offset* (max 0 (- *offset* 5)))
            (buf:cursor-prev-line (top (first (ed:layout *ui*))))
            ))
         ;; TODO clamp point to window
         )

        ;; TODO mouse dispatch
        (#.sdl:+sdl-mousemotion+
         (format t "mouse event ~d ~d ~%"
                 (cffi:foreign-slot-value event 'sdl:sdl-mouse-motion-event 'x)
                 (cffi:foreign-slot-value event 'sdl:sdl-mouse-motion-event 'y)))

        ;; TODO keydown dispatch
        (#.sdl:+sdl-keydown+
         ;; (buf:insert-at (win:buffer (first (ed:layout *ui*)))
         ;;                (point (first (ed:layout *ui*)))
         ;;                (string
         ;;                 (code-char
         ;;                  (cffi:foreign-slot-value
         ;;                   (cffi:foreign-slot-value event 'sdl:sdl-keyboard-event 'sdl:keysym)
         ;;                   'sdl:sdl-keysym 'sdl:sym))))
         )
        )
    (never-gonna-give-you-up ()
      (return-from handle-event))))

(defun main (file)
  (let* ((buffer (buf:buffer (probe-file file)))
         (top (buf:cursor buffer 0))
         (point (buf:cursor buffer 1)) ; TODO save position
         (window (make-instance 'sdl-window :point point :top top
                                            :buffer buffer)))
    (setf (aref (buf:cursors buffer) 0) top
          (aref (buf:cursors buffer) 1) point
          *ui* (make-instance 'sdl-frontend
                              :w 1280 :h 800
                              :layout `(,window)))
    (with-slots (%renderer %ttf-font %window %cwidth %cheight) *ui*
      (unwind-protect
           (progn
             (or (zerop (sdl:sdl-init sdl:+sdl-init-video+))
                 (format t "SDL failed to initialize: ~a~%" (sdl:sdl-get-error)))
             ;; ttf
             (sdl-ttf:ttf-init)
             (setf %ttf-font (sdl-ttf:ttf-open-font
                              (namestring (fonts:file (apply #'fonts:find-font (font *ui*))))
                              *font-size*))
             (when (cffi:null-pointer-p %ttf-font)
               (format t "SDL_ttf failed to initialize: ~a~%" (sdl:sdl-get-error))
               (return-from main))
             ;; FONT HACK
             (cffi:with-foreign-objects ((%w :int) (%h :int))
               (sdl-ttf:ttf-size-utf8 %ttf-font "i" %w %h)
               (setf %cwidth (cffi:mem-aref %w :int)
                     %cheight (cffi:mem-aref %h :int)))
             (format t "cwidth: ~d cheight: ~d~%" %cwidth %cheight)
             ;; window
             (setf %window (sdl:sdl-create-window "main window"
                                                  sdl:+sdl-windowpos-undefined+
                                                  sdl:+sdl-windowpos-undefined+
                                                  (ed:width *ui*) (ed:height *ui*) 0))
             (when (cffi:null-pointer-p %window)
               (format t "SDL window failed to initialize: ~a~%" (sdl:sdl-get-error))
               (return-from main))
             (format t "initialized SDL window~%")
             ;; renderer
             (setf %renderer (sdl:sdl-create-renderer %window -1
                                                      sdl:+sdl-renderer-accelerated+))
             (when (cffi:null-pointer-p %renderer)
               (format t "SDL renderer failed to initialize: ~a~%" (sdl:sdl-get-error))
               (return-from main))
             (apply #'sdl:sdl-set-render-draw-color %renderer *text-color*)
             (format t "initialized renderer~%")
             ;; main loop
             (cffi:with-foreign-object (event 'sdl:sdl-event)
               (loop :initially (setf (ed:running? *ui*) t)
                     :while (ed:running? *ui*)
                     :do (sdl:sdl-wait-event event) ; 10ms poll
                         (handle-event event)
                         (render))))
        ;; unwind
        (format t "stopped~%")
        (setf (ed:running? *ui*) nil)
        (when %window
          (sdl:sdl-destroy-window %window)
          (setf %window nil))
        (when %renderer
          (sdl:sdl-destroy-renderer %renderer)
          (setf %renderer nil))
        (when %ttf-font
          (sdl-ttf:ttf-close-font %ttf-font)
          (setf %ttf-font nil))
        (sdl-ttf:ttf-quit)
        (sdl:sdl-quit)
        ))))
