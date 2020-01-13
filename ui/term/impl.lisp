;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
;;
;; XXX the frontend impl should use a read-only clone

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
  (concurrency:without-interrupts
    (dolist (ui (frontends *editor*))
      (when (typep ui 'tui)
        (let* ((dimensions (term:get-terminal-dimensions))
               (xscale (/ (cdr dimensions) (width ui)))
               (yscale (/ (car dimensions) (height ui))))
          (setf (width ui) (cdr dimensions) (height ui) (car dimensions))
          (dolist (window (windows ui)) ;XXX boundary windows should be larger to fit term
            (setf (window-width window) (truncate (* (window-width window) xscale))
                  (window-height window) (truncate (* (window-height window) yscale))))
          (concurrency:with-local-interrupts
            (redisplay ui :force-p t))
          ;; there can only be one TUI - the one the user started with
          (return))))))

(cffi:defcallback sigwinch-handler :void ((signo :int))
  (declare (ignore signo))
  (handle-winch))

;;; defs

(defmethod start ((ui tui))
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl)
        (*terminal-io* *standard-output*))

    (push ui (frontends *editor*))

    (let (original-termios original-handler)
      (unwind-protect
           (progn ; TODO save alternate screen when supported
             (setf original-termios (term:setup-terminal-input))
             (ti:set-terminal (uiop:getenv "TERM"))
             (ti:tputs ti:clear-screen)
             (%tui-redisplay ui :force-p t)
             (catch 'quit-ui-loop
               (setf original-handler (c-signal +sigwinch+ (cffi:callback sigwinch-handler)))
               (loop
                 (queue-event
                  (event-queue *editor*)
                  (let ((event (term:read-terminal-event)))
                    (%tui-parse-event ui event)))))
             (deletef (frontends *editor*) ui))
        (when original-termios
          (term:restore-terminal-input original-termios))
        (ti:tputs ti:clear-screen) (finish-output)
        (when original-handler
          (c-signal +sigwinch+ original-handler))))))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (throw 'quit-ui-loop nil))))

(defun %tui-parse-event (tui event)
  (cond ((characterp event) ; TODO treat all cases - ECOND
         (cond ((char= event #\Page)
                (make-key-event :name :c-l
                                :window (focused-window tui)))
               ((char= event #\Etx)
                (make-key-event :name :c-c
                                :window (focused-window tui)))
               ((char= event #\Enq)
                (make-key-event :name :c-e
                                :window (focused-window tui)))
               ((char= event #\Em)
                (make-key-event :name :c-y
                                :window (focused-window tui)))
               ;; ((char= event #\0)
               ;;  (make-key-event :name :0
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\1)
               ;;  (make-key-event :name :1
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\2)
               ;;  (make-key-event :name :2
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\3)
               ;;  (make-key-event :name :3
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\4)
               ;;  (make-key-event :name :4
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\5)
               ;;  (make-key-event :name :5
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\6)
               ;;  (make-key-event :name :6
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\6)
               ;;  (make-key-event :name :6
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\7)
               ;;  (make-key-event :name :7
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\8)
               ;;  (make-key-event :name :8
               ;;                  :window (focused-window tui)))
               ;; ((char= event #\9)
               ;;  (make-key-event :name :9
               ;;                  :window (focused-window tui)))
               ))
        ((listp event))
        (t)))

;; TODO implement window abstraction with borders
;; TODO smarter redisplay computation - XXX buffers are assumed not to change rn
;; TODO handle empty file

(let ((redisplay-depth 0))
  ;; this is not the optimal approach - we should first decide whether to perform
  ;; optimization at all, otherwise clear-screen and start from scratch. if we do, we
  ;; can overwrite/clear artifacts in one step, line per line, perhaps scrolling first
  (defun %tui-redisplay (tui &key force-p)
    (let ((initial-depth (incf redisplay-depth))
          (start-time (get-internal-real-time)))
      (macrolet ((aborting-on-interrupt (&body body) ; this is stupid and doesn't work
                   `(progn
                      (when (< initial-depth redisplay-depth)
                        (decf redisplay-depth)
                        (return-from %tui-redisplay t))
                      ,@body)))
        ;; line iterator pls
        (dolist (window (windows tui))
          (let ((last-top (last-top-line window))
                (top (top-line window))
                start-line end-line
                visual-line)
            (cond ((or force-p (>= (abs (- last-top top)) (window-height window)))
                   (aborting-on-interrupt (ti:tputs ti:clear-screen))
                   (setf start-line top
                         end-line (+ top (1- (window-height window)))
                         visual-line 1))
                  ((= top last-top)
                   (return))
                  ((> top last-top) ;XXX assuming no height resize here - should check
                   (aborting-on-interrupt
                    (ti:tputs ti:change-scroll-region
                              (1- (window-y window))
                              (- (window-height window) 2))
                    (ti:tputs ti:parm-index (- top last-top)))
                   (setf start-line (+ last-top (1- (window-height window)))
                         end-line (+ top (1- (window-height window)))
                         visual-line (+ (1- (window-y window))
                                        (- (window-height window) (- top last-top)))))
                  (t ; last-top > top
                   (aborting-on-interrupt
                    (ti:tputs ti:change-scroll-region
                              (1- (window-y window))
                              (- (window-height window) 2))
                    (ti:tputs ti:parm-rindex (- last-top top)))
                   (setf start-line top
                         end-line last-top
                         visual-line (1- (+ (window-y window) (- last-top top))))))
            (setf end-line (min end-line (line-count (window-buffer window))))
            (loop :with buffer = (window-buffer window)
                  :for line :from start-line :to end-line
                  :for start-offset = (line-number-offset buffer start-line)
                    :then next-offset
                  :for next-offset = (line-number-offset buffer (1+ line))
                  :for text = (subseq buffer start-offset (1- next-offset))
                  :do (aborting-on-interrupt
                       (ti:tputs ti:cursor-address (1- visual-line) 0)
                       (write-string
                        (with-output-to-string (displayed-string)
                          (loop :with width = 0
                                :for c across text
                                :for (length displayed-char) = (multiple-value-list
                                                                (term:wide-character-width c))
                                :while (<= (incf width length) (window-width window))
                                :do (write-string displayed-char displayed-string)))))
                      (incf visual-line)))
          (aborting-on-interrupt
           (ti:tputs ti:cursor-address (1- (window-height window)) (1- (window-y window)))
           (format t "~C[48;2;50;200;100;38;2;0;0;0m" (code-char 27))
           (format t " - ")
           (format t "~A | " (or (buffer-name (window-buffer window)) "an unnamed buffer"))
           (format t "line: ~d/~d | " (top-line window) (line-count (window-buffer window)))
           (format t "C-e/C-y to scroll down/up, C-l redraws, C-c quits. hf ;) | ")
           (format t "redisplayed in ~5f secs" (/ (- (get-internal-real-time) start-time)
                                                  internal-time-units-per-second))
           (ti:tputs ti:clr-eol);assuming back-color-erase
           (format t "~C[m" (code-char 27));ecma-48 0 default parm - do terminals get it?
           (ti:tputs ti:cursor-address (1- (point-line window)) (1- (point-col window)))
           (force-output))))
      (decf redisplay-depth)
      nil)))

(defmethod redisplay ((ui tui) &key force-p)
  (bt:interrupt-thread (ui-thread ui) (lambda () (%tui-redisplay ui :force-p force-p))))

;; TODO must move %top-line using a mark - may end up anywhere after deletion
;; and search backwards for line start
(defclass tui-window (window)
  ((last-top-line :initform 1
                  :accessor last-top-line)
   (top-line :initform 1
             :accessor top-line)
   (point-line :initform 1
               :accessor point-line)
   (point-col :initform 1
              :accessor point-col) ;expressed in grapheme clusters
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

(defmethod (setf top-line) :before (new-value (window tui-window))
  (setf (last-top-line window) (top-line window)))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer))
