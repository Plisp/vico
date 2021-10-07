(defpackage #:vico-sdl
  (:use :cl :alexandria)
  (:import-from #:raw-bindings-sdl2 #:x #:y #:h #:w)
  (:local-nicknames (#:fonts #:org.shirakumo.font-discovery)
                    (#:sdl #:raw-bindings-sdl2)
                    (#:sdl-ttf #:raw-bindings-sdl2-ttf))
  (:export #:dmain #:main))
(in-package #:vico-sdl)

(defun fsv (ptr type slot-name)
  (cffi:foreign-slot-value ptr type slot-name))

(defun (setf fsv) (new-value ptr type slot)
  (setf (cffi:foreign-slot-value ptr type slot) new-value))

(defvar *running* nil)
(defvar *window* nil)
(defvar *renderer* nil)
(defvar *ttf-font* nil)

;; TODO use a buffer
(defparameter *text*
  "very very very loing bit of text very very loing bit of text1234
  second very very very loing bit of text
int main()
{
    return 0;
}")
(defparameter *offset* 0)

;; hmm... I might have to coordinate cursor/window states... multiple mutable object states

(defun %handle-event (event)
  "TODO handle pending events"
  (restart-case
      (case (fsv event 'sdl:sdl-event 'sdl:type)
        (#.sdl:+sdl-quit+
         (setf *running* nil))
        ;;
        (#.sdl:+sdl-mousemotion+
         (format t "ouse event ~d ~d ~%"
                 (fsv event 'sdl:sdl-mouse-motion-event 'x)
                 (fsv event 'sdl:sdl-mouse-motion-event 'y)))
        ;;
        (#.sdl:+sdl-mousewheel+
         (case (print (fsv event 'sdl:sdl-mouse-wheel-event 'y))
           (1 (incf *offset* 3))
           (-1 (setf *offset* (max 0 (- *offset* 3))))))
        ;; keydown
        (#.sdl:+sdl-keydown+
         ())
        )
    (never-gonna-give-you-up ()
      (return-from %handle-event))))

(defun %render ()
  (with-input-from-string (s *text*)
    (loop :with last-y = 0
          :for line = (read-line s nil nil)
          :while line
          :for first-line = t :then nil
          :for surface = (sdl-ttf:ttf-render-utf8-blended *ttf-font* line
                                                          '(sdl:r 147 sdl:g 161 sdl:b 161))
          :for texture = (sdl:sdl-create-texture-from-surface *renderer* surface)
          :do (cffi:with-foreign-object (rect 'sdl:sdl-rect)
                (setf (fsv rect 'sdl:sdl-rect 'x) 0
                      (fsv rect 'sdl:sdl-rect 'y) last-y
                      (fsv rect 'sdl:sdl-rect 'w) (fsv surface 'sdl:sdl-surface 'w)
                      (fsv rect 'sdl:sdl-rect 'h) (fsv surface 'sdl:sdl-surface 'h))
                (if first-line
                    (cffi:with-foreign-object (clip 'sdl:sdl-rect)
                      (cffi:with-foreign-slots ((x y w h) clip sdl:sdl-rect)
                        (setf x 0
                              y *offset*
                              w (fsv surface 'sdl:sdl-surface 'w)
                              h (- (fsv surface 'sdl:sdl-surface 'h) *offset*)))
                      (decf (fsv rect 'sdl:sdl-rect 'h) *offset*)
                      (sdl:sdl-render-copy *renderer* texture clip rect))
                    (sdl:sdl-render-copy *renderer* texture (cffi:null-pointer) rect))
                (incf last-y (fsv rect 'sdl:sdl-rect 'h)))
              (sdl:sdl-free-surface surface)
              (sdl:sdl-destroy-texture texture)
          :finally (sdl:sdl-render-present *renderer*))))

(defparameter *font* '(:family "InputSans" :weight 100))

(defun main ()
  (let ((width 800)
        (height 600)
        *window* *renderer* *ttf-font*)
    (unwind-protect
         (progn
           (or (zerop (sdl:sdl-init sdl:+sdl-init-video+))
               (format t "SDL failed to initialize: ~a~%" (sdl:sdl-get-error)))
           ;; ttf
           (sdl-ttf:ttf-init)
           (setf *ttf-font* (sdl-ttf:ttf-open-font
                             (namestring (fonts:file (apply #'fonts:find-font *font*)))
                             18))
           (when (cffi:null-pointer-p *ttf-font*)
             (format t "SDL *ttf-font* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           ;; *window*
           (setf *window* (sdl:sdl-create-window "main *window*"
                                                 sdl:+sdl-windowpos-undefined+
                                                 sdl:+sdl-windowpos-undefined+
                                                 width height 0))
           (when (cffi:null-pointer-p *window*)
             (format t "SDL *window* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           (format t "initialized SDL window~%")
           ;; *renderer*
           (setf *renderer* (sdl:sdl-create-renderer *window* -1
                                                     sdl:+sdl-renderer-accelerated+))
           (when (cffi:null-pointer-p *renderer*)
             (format t "SDL *renderer* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           (sdl:sdl-set-render-draw-color *renderer* 0 43 54 0)
           (format t "initialized renderer~%")
           ;; main loop
           (loop :initially (setf *running* t)
                 :while *running*
                 :do (cffi:with-foreign-object (event 'sdl:sdl-event)
                       (sdl:sdl-wait-event event)
                       (sdl:sdl-render-clear *renderer*)
                       (%handle-event event)
                       (%render))))
      ;; unwind
      (format t "stopped~%")
      (setf *running* nil)
      (when *window*
        (sdl:sdl-destroy-window *window*)
        (setf *window* nil))
      (when *renderer*
        (sdl:sdl-destroy-renderer *renderer*)
        (setf *renderer* nil))
      (when *ttf-font*
        (sdl-ttf:ttf-close-font *ttf-font*)
        (setf *ttf-font* nil))
      (sdl-ttf:ttf-quit)
      (sdl:sdl-quit))))
