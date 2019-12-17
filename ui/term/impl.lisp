;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
;;
;; TODO fix up the damn key-events
;;

(in-package :vico-term.impl)

(defclass tui (ui)
  ((width :initarg :width
          :accessor width)
   (height :initarg :height
           :accessor height)
   (focused-window :initarg :focused-window
                   :accessor focused-window
                   :type window)
   (windows :initform (list)
            :accessor windows
            :type list)))

;;; sigwinch handling

(defconstant +sigwinch+ 28)

(cffi:defcfun ("signal" c-signal) :pointer
  (signo :int)
  (handler :pointer))

(defun handle-winch ()
  (dolist (ui (frontends *editor*))
    (when (typep ui 'tui)
      (let* ((dimensions (term:get-terminal-dimensions))
             (xscale (/ (cdr dimensions) (width ui)))
             (yscale (/ (car dimensions) (height ui))))
        (mapcar
         (lambda (window)
           (setf (window-width  window) (truncate (* (window-width window)  xscale))
                 (window-height window) (truncate (* (window-height window) yscale))))
         (windows ui))
        (setf (width  ui) (cdr dimensions)
              (height ui) (car dimensions))
        (bt:interrupt-thread (ui-thread ui)
                             (lambda ()
                               (mapcar (lambda (window)
                                         (%term-redisplay window))
                                       (windows ui))))))))

(cffi:defcallback sigwinch-handler :void ((signo :int))
  (declare (ignore signo))
  (handle-winch))

;;; defs

(defmethod start ((ui tui))
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl)
        (*terminal-io* *standard-output*))

    (push ui (frontends *editor*))

    (let (orig-termios init-done original-handler)
      (unwind-protect
           (progn
             ;; (format t "~&~C[38;2;42;161;152m~
             ;;            C-e/C-y to scroll down/up, C-l redraws, C-c quits. glhf ;)~
             ;;            ~C[m"
             ;;         (code-char 27) (code-char 27))
             ;; (force-output)
             ;; (sleep 1.5)

             (setf orig-termios (term:setup-terminal-input))
             (ti:set-terminal (uiop:getenv "TERM"))
             (ti:tputs ti:clear-screen) ;TODO line wrap
             (mapcar (lambda (window) (%term-redisplay window)) (windows ui))

             (catch 'quit-ui-loop
               (loop
                 (%term-redisplay
                  (catch 'redisplay
                    (unless init-done
                      (setf original-handler (c-signal +sigwinch+ (cffi:callback sigwinch-handler)))
                      (setf init-done nil))
                    (loop
                      (queue-event (event-queue *editor*)
                                   (let ((ev (term:read-terminal-event)))
                                     (when (characterp ev)
                                       (cond ((char= ev #\Page)
                                              (make-key-event :name :c-l
                                                              :window (focused-window ui)))
                                             ((char= ev #\Etx)
                                              (make-key-event :name :c-c
                                                              :window (focused-window ui)))
                                             ((char= ev #\Enq)
                                              (make-key-event :name :c-e
                                                              :window (focused-window ui)))
                                             ((char= ev #\Em)
                                              (make-key-event :name :c-y
                                                              :window (focused-window ui))))))))))))
             (deletef (frontends *editor*) ui))

        (when orig-termios
          (term:restore-terminal-input orig-termios))
        (ti:tputs ti:clear-screen) (finish-output)
        (when original-handler
          (c-signal +sigwinch+ original-handler))))))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda ()
                                        (throw 'quit-ui-loop nil))))

;; TODO must move %top-line using a mark - may end up anywhere in a line after
;; deletion - thus search backwards for linefeed TODO buffer search interface
;; TODO initargs should be obvious from context?
(defclass tui-window (window)
  ((top-line :initform 1
             :accessor top-line)
   (point-line :initform 1
               :accessor point-line)
   (point-col :initform 1
              :accessor point-col)
   (x :initarg :x
      :accessor window-x)
   (y :initarg :y
      :accessor window-y)
   (width :initarg :width
          :accessor window-width)
   (height :initarg :height
           :accessor window-height)
   (buffer :initarg :buffer
           :accessor window-buffer))
  (:documentation "The only type of window"))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer))

;; TODO finish implementing window protocol
;; TODO implement window abstraction with borders
;; TODO smarter redisplay computation using edit history

(defun %term-redisplay (window)
  (loop :initially (ti:tputs ti:clear-screen)
                   (force-output)
        :for line from (top-line window)
        :for visual-line from 1 to (window-height window)

        :while (< line (line-count (window-buffer window)))
        :for line-offset = (line-number-offset (window-buffer window) line)
          then next-line-offset
        :for next-line-offset = (line-number-offset (window-buffer window) (1+ line))

        :for text = (subseq (window-buffer window) line-offset (1- next-line-offset))
        :do (ti:tputs ti:cursor-address (1- visual-line) 0)
            (write-string (subseq text 0 (min (window-width window) (length text))))
        :finally (loop while (<= visual-line (window-height window))
                       do (ti:tputs ti:cursor-address (1- visual-line) 0)
                          (princ #\~)
                          (incf visual-line))
                 (ti:tputs ti:cursor-address ; TODO make util
                           (1- (point-line window)) (1- (point-col window)))
                 (finish-output)))

(defmethod redisplay-window ((window tui-window) &key force-p)
  (declare (ignore force-p))
  (throw 'redisplay window))
