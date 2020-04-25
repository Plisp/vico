;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
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
        (setf (slot-value ui 'width) (cdr dimensions)
              (slot-value ui 'height) (car dimensions))
        (dolist (window (windows ui)) ;XXX boundary windows should be larger to fit term
          (setf (slot-value window 'width) (truncate (* (window-width window) xscale))
                (slot-value window 'height) (truncate (* (window-height window) yscale))))
        (redisplay ui :force-p t)
        ;; there can only be one TUI (for now) - the one the user started with
        (return)))))

(cffi:defcallback sigwinch-handler :void ((signo :int))
  (declare (ignore signo))
  (concurrency:without-interrupts
    (handle-winch)))

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
             (ti:tputs ti:enter-ca-mode)
             (format t "~C[48;2;0;43;54m" #\escape)
             (ti:tputs ti:clear-screen) ;XXX assuming back-color-erase
             (%tui-redisplay ui :force-p t)
             (catch 'quit-ui-loop
               (setf original-handler (c-signal +sigwinch+
                                                (cffi:callback sigwinch-handler)))
               (loop
                 (queue-event
                  (event-queue *editor*)
                  (let ((event (term:read-terminal-event)))
                    (%tui-parse-event ui event)))))
             (deletef (frontends *editor*) ui))
        (when original-termios
          (term:restore-terminal-input original-termios))
        (ti:tputs ti:exit-ca-mode)
        (finish-output)
        (when original-handler
          (c-signal +sigwinch+ original-handler))))))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (throw 'quit-ui-loop nil))))

(defun %tui-parse-event (tui event)
  (cond ((characterp event)
         (make-key-event :name (case event
                                 (#\Rubout :backspace)
                                 (#\Page :control-l)
                                 (#\Eot :control-d)
                                 (#\Etx :control-c)
                                 (#\So :control-n)
                                 (#\Dle :control-p)
                                 (#\Ack :control-f)
                                 (#\Stx :control-b)
                                 (#\Enq :control-e)
                                 (#\Em :control-y)
                                 (otherwise event))
                         :window (focused-window tui)))
        ((keywordp event)
         (case event ;XXX
           (:up (make-key-event :name :page-up :window (focused-window tui)))
           (:down (make-key-event :name :page-down :window (focused-window tui)))))
        (t
         (make-key-event :name :null :window (focused-window tui)))))

;; TODO implement window abstraction with borders
;; TODO portable terminal code

(defun update-style (current-style next-style)
  (when-let ((diff (hl:style-difference current-style next-style)))
    (flet ((attr-string (name attr)
             (case name
               (:fg (format nil "38;2;~d;~d;~d;"
                            (hl:r attr)
                            (hl:g attr)
                            (hl:b attr)))
               (:bg (format nil "48;2;~d;~d;~d;"
                            (hl:r attr)
                            (hl:g attr)
                            (hl:b attr)))
               (:bold (if attr "1;" "22;"))
               (:italic (if attr "3;" "23;"))
               (:underline (if attr "4;" "24;"))
               (otherwise ""))))
      (let ((s (with-output-to-string (s)
                 (format s "~C[" #\escape)
                 (loop :for (name attr) :on diff :by #'cddr
                       :do (write-string (attr-string name attr) s)))))
        (setf (aref s (1- (length s))) #\m) ; last #\;->#\m
        (write-string s)))))

(defun %tui-redisplay (tui &key force-p)
  (let ((start-time (get-internal-real-time)))
    (dolist (window (windows tui))
      (block draw-buffer
        (unless (zerop (length (window-buffer window)))
          (ti:tputs ti:change-scroll-region
                    (1- (window-y window))
                    (- (window-height window) 2))
          (let* ((buffer (window-buffer window))
                 (last-top (last-top-line window))
                 (top (window-top-line window))
                 (top-line-index (line-at top))
                 (delta (- (line-at top) (line-at last-top)))
                 start-line end-line
                 visual-line)
            (cond ((or force-p (>= (abs delta) (window-height window)))
                   (ti:tputs ti:clear-screen)
                   (setf start-line (line-at top)
                         end-line (+ (line-at top) (- (window-height window) 2))
                         visual-line 1))
                  ((zerop delta)
                   (return-from draw-buffer))
                  ((plusp delta)
                   (ti:tputs ti:parm-index delta)
                   (setf start-line (+ (line-at last-top) (1- (window-height window)))
                         end-line (+ (line-at top) (- (window-height window) 2))
                         visual-line (+ (1- (window-y window))
                                        (- (window-height window) delta))))
                  (t ; last-top > top
                   (ti:tputs ti:parm-rindex (- delta))
                   (setf start-line (line-at top)
                         end-line (1- (line-at last-top))
                         visual-line 1)))
            (setf end-line (min end-line (line-count buffer)))
            ;; (unless force-p
            ;;   (format t "~C[48;2;100;20;40m" #\escape))
            (loop :initially (if (> start-line (line-count buffer))
                                 (loop-finish)
                                 (cursor-next-line redraw (- start-line (line-at top))))
                             ;; (let ((cursor (window-point window)))
                             ;;   (unless (<= (line-at top)
                             ;;               (line-at cursor)
                             ;;               (1- (+ (line-at top) (1- (window-height window)))))
                             ;;     (cursor-move-to-line cursor top-line-index)))
                  :with redraw = (copy-cursor top)
                  :with current-style = hl:*default-style*
                  :until (> (line-at redraw) end-line)
                  :do (ti:tputs ti:cursor-address (1- visual-line) 0)
                      (let* ((start-of-line (copy-cursor redraw))
                             (line-text
                               (loop :with total-width = 0
                                     :with char
                                     :with width
                                     :while (< (index-at redraw) (length buffer))
                                     :do (setf char (char-at redraw)
                                               width (term:character-width char))
                                     :while (and (not (char= char #\newline))
                                                 (<= (incf total-width width)
                                                     (window-width window)))
                                     :do (cursor-next redraw)
                                     :finally (return
                                                (subseq-at start-of-line
                                                           (- (index-at redraw)
                                                              (index-at start-of-line))))))
                             (syntax (make-array (length line-text) :initial-element :text)))
                        (dolist (lexer (vico-core.standard-buffer::lexers buffer))
                          (funcall lexer buffer syntax line-text
                                   (index-at start-of-line)
                                   (+ (index-at start-of-line) (length line-text))))
                        (loop :for c :across line-text
                              :for idx :from 0
                              :do (let ((next-style (hl:syntax-style (svref syntax idx))))
                                    (update-style current-style next-style)
                                    (setf current-style next-style))
                                  (write-string (nth-value 1 (term:character-width c)))))
                      (incf visual-line)
                      (cursor-next-line redraw)
                  :finally (update-style current-style hl:*default-style*)
                           (cursor-move-line last-top delta)
                           (cursor-move-to-line top top-line-index)))))
      (ti:tputs ti:cursor-address (1- (window-height window)) (1- (window-y window)))
      (format t "~C[48;2;7;54;66m" #\escape)
      ;; per window status
      (let ((status-line
              (with-output-to-string (status-string)
                (format status-string " - ")
                (format status-string "~A | " (window-name window))
                (format status-string "cursor at line ~d | "
                        (line-at (window-point (focused-window tui))))
                (format status-string "redisplayed in ~5f secs | "
                        (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second))
                (format status-string "ctrl-l (re)draws screen, C-e/y scrolls window, ~
                                       C-c quits, up/down arrows = pageup/down, C-d deletes")
                )))
        (write-string ; XXX assuming ascii
         (subseq status-line 0 (min (length status-line) (window-width window))))
        ;;XXX assuming back-color-erase
        (ti:tputs ti:clr-eol)
        (format t "~C[48;2;0;43;54m" #\escape)))
    (let* ((focused (focused-window tui))
           (cursor (window-point focused))
           (column (- (index-at cursor) ;XXX variable width
                      (index-at (cursor-to-line-start (copy-cursor cursor))))))
      (ti:tputs ti:cursor-address (- (line-at cursor) (line-at (window-top-line focused)))
                column))
    (force-output))
  nil)

(defmethod redisplay ((ui tui) &key force-p)
  (bt:interrupt-thread (ui-thread ui) (lambda () (%tui-redisplay ui :force-p force-p))))

;; TODO must move %top-line using a cursor - may end up anywhere after deletion
;; and search backwards for line start

(defclass tui-window (window)
  ((last-top-line :initform (error "LAST-TOP-LINE cursor required")
                  :initarg :last-top-line
                  :reader last-top-line)
   (top-line :initform (error "TOP-LINE cursor required") ;3821
             :initarg :top-line
             :reader window-top-line)
   (point :initarg :point
          :reader window-point)
   (x :initarg :x
      :accessor window-x)
   (y :initarg :y
      :accessor window-y)
   (width :initarg :width
          :accessor window-width)
   (height :initarg :height
           :accessor window-height)
   (buffer :initarg :buffer
           :accessor window-buffer)
   (name :initarg :name
         :accessor window-name))
  (:documentation "The only type of window"))

(defun %tui-clamp-window-to-cursor (window point)
  ;; cursor beyond window
  (let ((delta (- (buf:line-at point)
                  (+ (buf:line-at (window-top-line window))
                     (- (window-height window) 2)))))
    (when (plusp delta)
      (%tui-scroll-window window delta)))
  ;; cursor before window
  (let ((delta (- (buf:line-at (window-top-line window))
                  (buf:line-at point))))
    (when (plusp delta)
      (%tui-scroll-window window (- delta)))))

(defun %tui-clamp-cursor-to-window (window point)
  ;; cursor beyond window
  (let ((delta (- (buf:line-at point)
                  (+ (buf:line-at (window-top-line window))
                     (- (window-height window) 2)))))
    (when (plusp delta)
      (move-point-lines window (- delta))))
  ;; cursor before window
  (let ((delta (- (buf:line-at (window-top-line window))
                  (buf:line-at point))))
    (when (plusp delta)
      (move-point-lines window delta))))

(defun %tui-scroll-window (window lines)
  (cursor-move-line (window-top-line window) lines))

(defmethod scroll-window ((window tui-window) lines)
  (%tui-scroll-window window lines)
  (%tui-clamp-cursor-to-window window (window-point window))
  (redisplay (window-ui window))
  (values))

;; TODO there is a problem with concurrency - locking needed across whole operation
;; to avoid out of bounds/incorrect results if intersected by writing on main thread
;; not a problem with most user code - is it an issue in practice anyways?

(defun %tui-move-point (window count)
  (let ((cursor (window-point window)))
    (if (plusp count)
        (loop :repeat count
              :until (= (buf:index-at cursor) (buf:length (window-buffer window)))
              :do (buf:cursor-next cursor))
        (loop :repeat (- count)
              :until (zerop (buf:index-at cursor))
              :do (buf:cursor-prev cursor)))))

(defmethod move-point ((window tui-window) &optional count)
  (%tui-move-point window count)
  (%tui-clamp-window-to-cursor window (window-point window))
  (redisplay (window-ui window))
  (window-point window))

(defmethod move-point-lines ((window tui-window) &optional count)
  (let* ((point (window-point window))
         (columns (- (index-at point) ;XXX variable width
                     (index-at (cursor-to-line-start (copy-cursor point)))))
         (buffer (window-buffer window)))
    (if (plusp count)
        (unless (= (buf:line-at point) (buf:line-count buffer))
          (loop :repeat count
                :until (= (buf:line-at point) (buf:line-count buffer))
                :do (buf:cursor-next-line point)
                :finally (%tui-clamp-window-to-cursor window point)
                         (loop :repeat columns
                               :while (and (< (buf:index-at point) (buf:length buffer))
                                           (char/= (char-at point) #\newline))
                               :do (%tui-move-point window 1))))
        (unless (= (buf:line-at point) 1)
          (loop :repeat (- count)
                :until (= (buf:line-at point) 1)
                :do (buf:cursor-prev-line point)
                :finally (%tui-clamp-window-to-cursor window point)
                         (loop :repeat columns
                               :while (and (< (buf:index-at point) (buf:length buffer))
                                           (char/= (char-at point) #\newline))
                               :do (%tui-move-point window 1)))))
    (redisplay (window-ui window))
    point))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer
                             :last-top-line (make-cursor buffer 0)
                             :top-line (make-cursor buffer 0)
                             :point (make-cursor buffer 0)
                             :name (or (buffer-name buffer) "an unnamed buffer")))
