;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
;;
;; XXX the frontend impl must use read-only cloning in the presence of multiple frontends

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
          (redisplay ui)
          ;; there can only be one TUI - the one the user started with
          ;; if this is called, that UI is live and triggered this signal
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
             (ti:tputs ti:clear-screen)
             (%tui-redisplay ui)
             (catch 'quit-ui-loop
               (loop
                  (%tui-redisplay
                   (catch 'redisplay
                     (unless init-done
                       (setf original-handler
                             (c-signal +sigwinch+ (cffi:callback sigwinch-handler)))
                       (setf init-done nil))
                     (loop
                        (queue-event
                         (event-queue *editor*)
                         (let ((ev (term:read-terminal-event)))
                           (cond ((characterp ev) ; TODO treat all cases - ECOND
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
                                                         :window (focused-window ui)))
                                        ;; ((char= ev #\0)
                                        ;;  (make-key-event :name :0
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\1)
                                        ;;  (make-key-event :name :1
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\2)
                                        ;;  (make-key-event :name :2
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\3)
                                        ;;  (make-key-event :name :3
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\4)
                                        ;;  (make-key-event :name :4
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\5)
                                        ;;  (make-key-event :name :5
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\6)
                                        ;;  (make-key-event :name :6
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\6)
                                        ;;  (make-key-event :name :6
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\7)
                                        ;;  (make-key-event :name :7
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\8)
                                        ;;  (make-key-event :name :8
                                        ;;                  :window (focused-window ui)))
                                        ;; ((char= ev #\9)
                                        ;;  (make-key-event :name :9
                                        ;;                  :window (focused-window ui)))
                                        ))
                                 ((listp ev))))))))))
             (deletef (frontends *editor*) ui))

        (when orig-termios
          (term:restore-terminal-input orig-termios))
        (ti:tputs ti:clear-screen) (finish-output)
        (when original-handler
          (c-signal +sigwinch+ original-handler))))))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (throw 'quit-ui-loop nil))))

;; TODO implement window abstraction with borders
;; TODO smarter redisplay computation

(let ((abortp nil)) ; only set in this function from the TUI thread
  (defun %tui-redisplay (tui)
    (ti:tputs ti:clear-screen)
    (dolist (window (windows tui))
      (do* ((buffer (window-buffer window))
            (line (top-line window) (1+ line))
            (visual-line 1 (1+ visual-line))
            (line-offset (line-number-offset (window-buffer window) line))
            (next-line-offset line-offset)
            (text))
           ((or (> visual-line (window-height window))
                (= line (line-count buffer)))
            (do ()
                ((> visual-line (window-height window)))
              ;; io about to be done. STOP!!! We may have been interrupted and should quit
              (concurrency:without-interrupts
                ()
                (ti:tputs ti:cursor-address (1- visual-line) 0)
                (princ #\~))
              (incf visual-line)))
        (setf line-offset next-line-offset
              next-line-offset (line-number-offset (window-buffer window) (1+ line))
              text (subseq (window-buffer window) line-offset (1- next-line-offset)))
        ;; io about to be done. STOP!!!
        (concurrency:without-interrupts
          ()
          (ti:tputs ti:cursor-address (1- visual-line) 0)
          (write-string
           (with-output-to-string (displayed-string)
             (loop :with width = 0
                   :for c across text
                   :for (length displayed-char) = (multiple-value-list
                                                   (term:wide-character-width c))
                   :while (<= (incf width length) (window-width window))
                   :do (write-string displayed-char displayed-string))))))
      (force-output))
    nil))

(defmethod redisplay ((ui tui) &key force-p)
  (declare (ignore force-p))
  (bt:interrupt-thread (ui-thread ui) (lambda () (throw 'redisplay nil))))

;; TODO must move %top-line using a mark - may end up anywhere after deletion
;; and search backwards for line start
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
