;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
;;

(in-package :vico-term.impl)

(defclass tui (term:tui ui)
  ()
  (:default-initargs :event-handler #'tui-handle-event))

(defclass vico-tui-window (term:standard-window window)
  ((last-top-line :accessor last-top-line)
   (max-point-col :accessor max-point-col)
   (last-edit-time :initform 0 :accessor last-edit-time)
   ;; general
   (top-line :initarg :top-line
             :initform nil
             :accessor window-top-line)
   (point :initarg :point
          :initform nil
          :accessor window-point)
   (point-col :initform 0)
   (buffer :initarg :buffer
           :initform (error "window buffer not provided")
           :accessor window-buffer)
   (name :initarg :name
         :initform "a +2 unnamed buffer"
         :accessor window-name)
   (cursor :accessor cursor))
  (:documentation "The only type of window"))

;;; tui

(defmethod height ((ui tui))
  (term:lines ui))
(defmethod (setf height) (new-value (ui tui))
  (setf (term:lines ui) new-value))

(defmethod width ((ui tui))
  (term:columns ui))
(defmethod (setf width) (new-value (ui tui))
  (setf (term:columns ui) new-value))

(defmethod focused-window ((ui tui))
  (term:focused-window ui))
(defmethod (setf focused-window) (new-value (ui tui))
  (setf (term:focused-window ui) new-value))

(defmethod windows ((ui tui))
  (term:windows ui))
(defmethod (setf windows) (new-value (ui tui))
  (setf (term:windows ui) new-value))

(defmethod term:run ((ui tui))
  (with-accessors ((canvas term::canvas)
                   (screen term::screen)
                   (lines term:lines)
                   (columns term:columns)
                   (focused-window focused-window)
                   (event-handler term:event-handler))
      ui
    (setf canvas (make-array (list lines columns)))
    (setf screen (make-array (list lines columns)))
    (symbol-macrolet ((new-cell (make-instance 'term:cell)))
      (loop :for idx :below (array-total-size canvas)
            :do (setf (row-major-aref canvas idx) new-cell
                      (row-major-aref screen idx) new-cell)))
    (term:enable-alternate-screen)
    (term:clear-screen)
    (term:enable-mouse)
    (term:set-cursor-shape :bar :blink-p t)
    (term:catch-sigwinch)
    (term:redisplay ui)
    (term:set-cursor-position 0 0)
    (finish-output)
    (unwind-protect
         (catch 'vico-tui-quit
           (setf (running-p ui) t)
           (loop
             (let ((event (term:read-event)))
               (when (term:got-winch ui)
                 (term:handle-resize ui))
               (funcall event-handler ui event))))
      (setf (running-p ui) nil)
      (term:disable-mouse)
      (term:set-cursor-shape :block)
      (term:disable-alternate-screen)
      (loop :while (term:read-event-timeout 0)) ; drain events
      (term:reset-sigwinch)
      (finish-output))))

(defmethod term:stop ((ui tui))
  (throw 'vico-tui-quit nil))

(defmethod start ((ui tui))
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl) (*terminal-io* *standard-output*))
    (push ui (frontends *editor*))
    (term:run ui)
    (deletef (frontends *editor*) ui)))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (term:stop ui))))

;; XXX lazy
(defun tui-handle-event (tui event)
  (let* ((focused-window (focused-window tui))
         (canonicalized
          (cond ((characterp event)
                 (log-event (format nil "got char: ~a" (char-name event)))
                 (make-key-event :name (case event
                                         (#\rubout :backspace)
                                         (#\backspace :backspace)
                                         (#\page :control-l)
                                         (#\eot :control-d)
                                         (#\etx :control-c)
                                         (#\so :control-n)
                                         (#\dc3 :control-s)
                                         (#\dle :control-p)
                                         (#\ack :control-f)
                                         (#\stx :control-b)
                                         (#\soh :control-a)
                                         (#\enq :control-e)
                                         (#\em :control-y)
                                         (#\dc4 :control-t)
                                         (#\etb :control-w)
                                         (otherwise event))
                                 :window (focused-window tui)))
                ((keywordp event)
                 (log-event event)
                 (case event
                   (:up (make-key-event :name :control-p :window focused-window))
                   (:down (make-key-event :name :control-n :window focused-window))
                   (:left (make-key-event :name :control-b :window focused-window))
                   (:right (make-key-event :name :control-f :window focused-window))
                   (:page-up (make-key-event :name :page-up :window focused-window))
                   (:page-down (make-key-event :name :page-down :window focused-window))))
                ((and (listp event) (search "SCROLL" (string (first event))))
                 (log-event event)
                 (case (first event)
                   (:scroll-up (make-key-event :name :control-y :window focused-window))
                   (:scroll-down (make-key-event :name :control-t :window focused-window))))
                ((and (listp event) (search "META" (string (first event))))
                 (log-event event)
                 (case (cdr event)
                   (#\b (make-key-event :name :alt-b :window focused-window))
                   (#\f (make-key-event :name :alt-f :window focused-window))))
                (t
                 (make-key-event :name :null :window focused-window)))))
    (queue-event (event-queue *editor*) canonicalized)))

;; TODO implement window abstraction with borders

(defun update-style (current-style next-style)
  (when-let ((diff (hl:style-difference current-style next-style)))
    (flet ((attr-string (name attr)
             (case name
               (:fg (format nil "38;2;~d;~d;~d;"
                            (hl:red attr)
                            (hl:green attr)
                            (hl:blue attr)))
               (:bg (format nil "48;2;~d;~d;~d;"
                            (hl:red attr)
                            (hl:green attr)
                            (hl:blue attr)))
               (:bold (if attr "1;" "22;"))
               (:italic (if attr "3;" "23;"))
               (:underline (if attr "4;" "24;"))
               (otherwise ""))))
      (let ((s (with-output-to-string (s)
                 (format s "~c[" #\esc)
                 (loop :for (name attr) :on diff :by #'cddr
                       :do (write-string (attr-string name attr) s)))))
        (setf (aref s (1- (length s))) #\m) ; last #\;->#\m
        (write-string s)))))

(defun tui-draw-window-status (window) ;TODO make generic for sure
  ;; per window status
  (let* ((status-line
           (with-output-to-string (status-string)
             (format status-string " - ")
             (format status-string "~a | " (window-name window))
             ;; don't display this by default (in large fragged files), not efficient
             (unless (> (buf:size (window-buffer window)) #.(expt 10 7))
               (format status-string "line ~d/~d | "
                       (buf:line-at (window-point window))
                       (buf:line-count (window-buffer window))))
             (format status-string "C-l (re)draws screen, ~
                                   mousewheel scrolls viewport, ~
                                   C-w quits, ~
                                   some emacs bindings"
                     )
             ;;(format status-string " | index ~d" (buf:index-at (window-point window)))
             ))
         (length (min (length status-line) (window-width window))))
    (term:puts (subseq status-line 0 length)
               (window-height window)
               1
               (term:make-style :bg #x073642))
    (term:puts (make-string (- (window-width window) length) :initial-element #\space)
               (window-height window)
               (1+ length)
               (term:make-style :bg #x073642))))

(defmethod term:redisplay :before ((ui tui))
  (log-event :hi)
  (loop :with canvas = (term::canvas ui)
        :for idx below (array-total-size canvas)
        :do (setf (term:cell-style (row-major-aref canvas idx))
                  (term:make-style :bg #x002b36))))

(defmethod redisplay ((ui tui) &key force-p)
  (declare (ignore force-p))
  (map () #'tui-clamp-window-to-cursor (windows ui))
  (when (running-p ui)
    (bt:interrupt-thread (ui-thread ui)
                         (lambda ()
                           (concurrency:without-interrupts
                             (handler-case (term:redisplay ui)
                               ;; will occur before anything is drawn
                               ;; during PRESENTing
                               (conditions:vico-cursor-invalid ()))
                             (apply #'term:set-cursor-position (cursor (focused-window ui)))
                             (finish-output))))))

;;; window

;; TODO don't use the actual cursor, represent as selection
;; selections can be optimized for one character, second cursor static
;; some function list will exist for obtaining style spans (cursor,cursor,style)

(defmethod term:present ((window vico-tui-window))
  "This routine may not modify any window parameters, as it does not run on the main
thread and may race."
  (with-accessors ((height window-height)
                   (width window-width)
                   (buffer window-buffer)
                   (point window-point)
                   (top-line window-top-line)
                   (last-top-line last-top-line)
                   (last-edit-time last-edit-time)
                   (cursor cursor))
      window
    (loop :with buffer-edit-time = (buf:edit-timestamp buffer)
          :with orig-top = (buf:copy-cursor top-line)
          :with top = (buf:cursor-bol (buf:copy-cursor orig-top))
          :with visual-end = (1- height)
          :with visual-line = 1
          :with current-style = hl:*default-style*
          :until (> visual-line visual-end)
          :do (loop :with column = 1
                    :for char = (buf:char-at top)
                    :for char-width = (term:character-width char)
                    :do (when (buf:cursor= point top)
                          (setf cursor (list (1- visual-line) (1- column))))
                        (unless (char= char #\newline)
                          (term:put char visual-line column
                                    (term:make-style :fg #xfdf6e3 :bg #x002b36)))
                    :while (and (not (char= char #\newline))
                                (<= (incf column char-width) width))
                    :do (handler-case
                            (buf:cursor-next-char top)
                          (conditions:vico-bad-index ()
                            (loop-finish)))
                    :finally (incf visual-line))
              (handler-case
                  (buf:cursor-next-line top)
                (conditions:vico-bad-line-number ()
                  (loop :until (> visual-line visual-end)
                        :do (term:put #\~ visual-line 1)
                            (incf visual-line))
                  (loop-finish)))
          :finally (update-style current-style hl:*default-style*)
                   (setf last-edit-time buffer-edit-time
                         last-top-line orig-top)
                   (tui-draw-window-status window))))

(defmethod window-x ((window vico-tui-window))
  (term:rect-x (term:dimensions window)))
(defmethod window-y ((window vico-tui-window))
  (term:rect-y (term:dimensions window)))
(defmethod window-width ((window vico-tui-window))
  (term:rect-cols (term:dimensions window)))
(defmethod window-height ((window vico-tui-window))
  (term:rect-rows (term:dimensions window)))

(defmethod initialize-instance :after ((window vico-tui-window) &key &allow-other-keys)
  (with-accessors ((last-top-line last-top-line)
                   (top-line window-top-line)
                   (point window-point)
                   (buffer window-buffer)
                   (max-point-col max-point-col))
      window
    (or top-line (setf top-line (buf:make-cursor buffer 0 :track t :static t)))
    (or point (setf point (buf:make-cursor buffer 0 :track t)))
    (setf max-point-col (buf:cursor- point (buf:cursor-bol (buf:copy-cursor point)))
          last-top-line (buf:copy-cursor top-line))))

(defun tui-clamp-window-to-cursor (window)
  (with-accessors ((point window-point)
                   (top-line window-top-line)
                   (height window-height))
      window
    (cond ((buf:cursor> point (buf:move-cursor-lines* (buf:copy-cursor top-line)
                                                      (- height 2)))
           (buf:move-cursor-to top-line (buf:move-cursor-lines* (buf:copy-cursor point)
                                                                (- (- height 2)))))
          ((buf:cursor< point top-line)
           (buf:move-cursor-to top-line point)))))

(defun tui-clamp-cursor-to-window (window)
  (with-accessors ((point window-point)
                   (top-line window-top-line)
                   (height window-height))
      window
    (cond ((buf:cursor> point (buf:move-cursor-lines* (buf:copy-cursor top-line)
                                                      (- height 2)))
           (buf:move-cursor-to point (buf:move-cursor-lines* (buf:copy-cursor top-line)
                                                             (- height 2))))
          ((buf:cursor< point top-line)
           (buf:move-cursor-to point top-line)))))

(defmethod scroll-window ((window vico-tui-window) lines)
  (buf:move-cursor-lines* (window-top-line window) lines)
  (tui-clamp-cursor-to-window window))

;; TODO grapheme clusters
(defmethod move-point ((window vico-tui-window) &optional (count 1))
  (with-accessors ((point window-point))
      window
    (buf:move-cursor-chars* point count)
    (setf (max-point-col window)
          (buf:cursor- point (buf:cursor-bol (buf:copy-cursor point))))))

;; can we handle this?: ཧྐྵྨླྺྼྻྂ
;; TODO record column, use display width
(defmethod move-point-lines ((window vico-tui-window) &optional (count 1))
  (with-accessors ((point window-point)
                   (max-point-col max-point-col)
                   (buffer window-buffer))
      window
    (buf:move-cursor-lines* point count)
    (loop :with max-col = max-point-col
          :until (or (zerop max-col)
                     (= (buf:index-at point) (buf:size buffer))
                     (char= (buf:char-at point) #\newline))
          :do (buf:cursor-next point)
              (decf max-col))))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'vico-tui-window :ui ui
                                  :dimensions (term:make-rectangle :x x
                                                                   :y y
                                                                   :cols width
                                                                   :rows height)
                                  :buffer buffer
                                  ;;:name (buf:buffer-filename buffer)
                                  ))
