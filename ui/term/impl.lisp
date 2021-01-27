;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
;;

(in-package :vico-term.impl)

(defclass tui (term:tui ui)
  ()
  (:default-initargs :event-handler #'tui-handle-event))

(defclass tui-window (term:standard-window window)
  ((point-column :accessor window-point-column)
   ;; general
   (top-line :initarg :top-line
             :initform nil
             :accessor window-top-line)
   (point :initarg :point
          :initform nil
          :accessor window-point)
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
  (term:rows ui))
(defmethod (setf height) (new-value (ui tui))
  (setf (term:rows ui) new-value))

(defmethod width ((ui tui))
  (term:cols ui))
(defmethod (setf width) (new-value (ui tui))
  (setf (term:cols ui) new-value))

(defmethod focused-window ((ui tui))
  (term:focused-window ui))
(defmethod (setf focused-window) (new-value (ui tui))
  (setf (term:focused-window ui) new-value))

(defmethod windows ((ui tui))
  (term:windows ui))
(defmethod (setf windows) (new-value (ui tui))
  (setf (term:windows ui) new-value))

;; TODO send an event
(defmethod term:handle-resize progn ((ui tui))
  ui)

(defmethod term:initialize :after ((ui tui))
  (setf (running-p ui) t)
  (push ui (ev:frontends ev:*editor*))
  (term:set-cursor-shape :bar :blink-p t))

(defmethod start ((ui tui))
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl) (*terminal-io* *standard-output*))
    (term:run ui)
    (deletef (ev:frontends ev:*editor*) ui)))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (term:stop ui))))

(defun tui-handle-event (tui event)
  (let* ((focused-window (focused-window tui))
         (canonicalized
           (key:make-key-event
            :window focused-window
            :name (etypecase event
                    (character event)
                    (keyword
                     (case event
                       (:up-arrow :control-p)
                       (:down-arrow :control-n)
                       (:left-arrow :control-b)
                       (:right-arrow :control-f)
                       (:page-up :page-up)
                       (:page-down :page-down)))
                    (list
                     (let ((name (first event)))
                       (typecase name
                         (character
                          (make-keyword
                           (apply #'concatenate 'string
                                  `(,@(mapcar #'string (cdr event))
                                    "-" ,(string-upcase name)))))
                         (keyword
                          (case (first event)
                            (:wheel-up :control-y)
                            (:wheel-down :control-t)
                            (:left event))))))))))
    (when canonicalized
      (ev:queue-event (ev:event-queue ev:*editor*) (ev:log-event canonicalized)))
    t))

(defun update-style (current-style next-style)
  (when-let ((diff (hl:style-difference current-style next-style)))
    (flet ((attr-string (name attr)
             (case name
               (:fg (if attr
                        (format nil "38;2;~d;~d;~d;"
                                (hl:red attr)
                                (hl:green attr)
                                (hl:blue attr))
                        (format nil "39;")))
               (:bg (if attr
                        (format nil "48;2;~d;~d;~d;"
                                (hl:red attr)
                                (hl:green attr)
                                (hl:blue attr))
                        (format nil "49;")))
               (:bold (if attr "1;" "22;"))
               (:italic (if attr "3;" "23;"))
               (:reverse (if attr "7;" "27;"))
               (:underline (if attr "4;" "24;"))
               (otherwise ""))))
      (let ((s (with-output-to-string (s)
                 (format s "~c[" #\esc)
                 (loop :for (name attr) :on diff :by #'cddr
                       :do (write-string (attr-string name attr) s)))))
        (setf (aref s (1- (length s))) #\m) ; last #\;->#\m
        (write-string s))))
  next-style)

(defun tui-draw-window-status (window) ;TODO determine based on buffer type
  ;; per window status
  (let* ((status-line
           (with-output-to-string (status-string)
             (format status-string " - ")
             (format status-string "~a | " (window-name window))
             (let ((point (window-point window)))
               (format status-string "line: ~d column: ~d"
                       (buf:line-at point)
                       (buf:cursor- point (buf:cursor-bol (buf:copy-cursor point)))))))
         (length (min (length status-line) (window-width window))))
    (term:puts (subseq status-line 0 length)
               (window-height window)
               1
               (term:make-style :fg (hl:fg hl:*default-style*)
                                :bg #x073642))
    (term:puts (make-string (- (window-width window) length) :initial-element #\space)
               (window-height window)
               (1+ length)
               (term:make-style :fg (hl:fg hl:*default-style*)
                                :bg #x073642))))

(defun style-to-term (style)
  (term:make-style :fg (hl:fg style)
                   :bg (hl:bg style)
                   :boldp (hl:boldp style)
                   :italicp (hl:italicp style)
                   :reversep (hl:reversep style)
                   :underlinep (hl:underlinep style)))

(defmethod term:redisplay :around ((ui tui))
  (let ((uncursed:*default-style* (style-to-term hl:*default-style*)))
    (call-next-method)))

(defmethod term:redisplay :after ((ui tui))
  (apply #'uncursed-sys::set-cursor-position (cursor (focused-window ui))))

;; TODO unfortunately redisplays occur twice due to activity on both stdin and wakeup
;; It is strictly necessary on wakeup. Do we copy most of the code from UNCURSED?
(defmethod redisplay ((ui tui) &key force-p)
  (declare (ignore force-p))
  (map () #'tui-clamp-window-to-cursor (windows ui))
  (ev:log-event :requested-redisplay)
  (when (running-p ui)
    (term:wakeup ui)))

;;; window

(defun char-display-width (char)
  (let ((char-width (term:character-width char)))
    (if (> char-width -1)
        (values char-width t)
        (if (char= char #\tab) ; non-printable
            8 ; TODO configurable
            (let ((code (char-code char)))
              (if (or (<= 0 code 31) (= code 127))
                  2
                  1))))))

(defmethod window-char-width ((window tui-window) char)
  (declare (ignore window))
  (char-display-width char))

(defmethod window-string-width ((window tui-window) string)
  (declare (ignore window))
  (reduce #'+ string :key #'char-display-width))

;; TODO move this hunk elsewhere
(defclass keyword-highlighting-buffer (buf:buffer) ())

(defparameter *max-word-lookaround* 50)
(defun word-at-point (point)
  (let ((copy (buf:copy-cursor point)))
    (handler-case
        (unless (char= (buf:char-at copy) #\newline)
          (buf:move-cursor-chars* copy 1))
      (conditions:vico-bad-index ()))
    (buf:cursor-search-prev copy "\\n|\\w+" *max-word-lookaround*)
    (let ((length (buf:cursor-search-next copy "\\w+" *max-word-lookaround*)))
      (when (and length
                 (buf:cursor>= point copy)
                 (buf:cursor<= point (buf:cursor-next-char copy length)))
        (buf:cursor-prev-char copy length)
        (buf:subseq-at copy length)))))

(defmethod buffer-styles-for-window append ((buffer keyword-highlighting-buffer)
                                            start end window)
  (let (spans)
    (flet ((add-span (start-cursor end-cursor)
             (push (make-instance 'style-span
                                  :start start-cursor
                                  :end end-cursor
                                  :style (hl:make-style :bg #x073642 :fg #x2aa198))
                   spans)))
      (when-let (word (word-at-point (window-point window)))
        (setf word (concatenate 'string "\\b" word "\\b"))
        (loop :with it = (buf:copy-cursor start)
              :with left = (buf:cursor- end start)
              :for old = (buf:index-at it)
              :do (if-let (length (buf:cursor-search-next it word left))
                    (progn
                      (add-span (buf:copy-cursor it)
                                (buf:copy-cursor (buf:cursor-next-char it length)))
                      (decf left (- (buf:index-at it) old)))
                    (return spans)))))))

(defmethod term:handle-mouse-event ((window tui-window) ui button state line col
                                    &key &allow-other-keys)
  (declare (ignore window ui button state line col))
  )

(defmethod term:handle-key-event ((window tui-window) ui event)
  (declare (ignore window ui event))
  )

(defmethod term:present ((window tui-window))
  "This routine may not modify any window parameters, as it does not run on the main
thread and may race."
  (let ((b (get-internal-real-time)))
    (with-accessors ((height window-height)
                     (width window-width)
                     (buffer window-buffer)
                     (point window-point)
                     (top-line window-top-line)
                     (cursor cursor))
        window
      (loop :with top = (buf:cursor-bol (buf:copy-cursor top-line))
            :with visual-end = (1- height)
            :with visual-line = 1
            :with current-style = hl:*default-style*
            :until (> visual-line visual-end)
            :do (loop :initially (when-let (first (first styles))
                                   (setf current-style
                                         (update-style hl:*default-style* first)))
                      :with end = (let ((cursor (buf:copy-cursor top)))
                                    (handler-case
                                        (loop :for char = (buf:char-at cursor)
                                              :for width-traversed = 0
                                                :then (+ width-traversed
                                                         (char-display-width char))
                                              :do (buf:cursor-next-char cursor)
                                              :until (or (> width-traversed width)
                                                         (char= char #\newline))
                                              :finally (return cursor))
                                      (conditions:vico-bad-index ()
                                        (buf:move-cursor-to cursor (buf:size buffer)))))
                      :with spans = (sort (styles-for-window window top end)
                                          #'buf:cursor<
                                          :key #'buf:span-start)
                      :with styles = (sort
                                      (remove-if-not
                                       #'(lambda (span)
                                           (and (buf:cursor< (buf:span-start span) top)
                                                (buf:cursor> (buf:span-end span) top)))
                                       spans)
                                      #'buf:cursor< :key #'buf:span-end)
                      :with column = 1
                      :with char
                      :with last-width = 0
                      :with display-width
                      :with printablep
                      :do (when (buf:cursor= point top)
                            (setf cursor (list (1- visual-line) (1- (+ column last-width)))))
                          (loop
                            :while
                            (when-let (span (first spans))
                              (when (buf:cursor= top (buf:span-start span))
                                (let ((style (span-style span)))
                                  (assert style)
                                  (setf current-style (update-style current-style style))
                                  (pop spans)
                                  (push span styles)
                                  (setf styles ; TODO need a proper min stack
                                        (sort styles #'buf:cursor< :key #'buf:span-end))))))
                          (loop
                            :while
                            (when-let (s (first styles))
                              (when (buf:cursor>= top (buf:span-end s))
                                (pop styles)
                                (if-let (prev (first styles))
                                  (let ((prev-style (span-style prev)))
                                    (assert prev-style)
                                    (setf current-style
                                          (update-style current-style prev-style)))
                                  (setf current-style
                                        (update-style current-style hl:*default-style*))))))
                          (setf char (handler-case
                                         (buf:char-at top)
                                       (conditions:vico-bad-index ()
                                         (loop-finish))))
                          (multiple-value-setq (display-width printablep)
                            (char-display-width char))
                          (when (plusp display-width)
                            (incf column last-width)
                            (setf last-width display-width))
                      :until (or (> (+ column (max 0 (1- display-width))) width)
                                 (= (buf:index-at top) (buf:size buffer)))
                      :do (if printablep
                              (term:put char
                                        visual-line column
                                        (style-to-term current-style))
                              (case char
                                (#\tab
                                 (term:puts "        "
                                            visual-line column
                                            (style-to-term current-style)))
                                (#\newline
                                 (loop-finish))
                                (otherwise
                                 (let ((code (char-code char)))
                                   (if (or (<= 0 code 31) (= code 127))
                                       (term:puts (format nil "^~c"
                                                          (code-char
                                                           (logxor #x40 (char-code char))))
                                                  visual-line column
                                                  (style-to-term current-style))
                                       (term:put #.(code-char #xfffd)
                                                 visual-line column
                                                 (style-to-term current-style)))))))
                          (handler-case
                              (buf:cursor-next-char top)
                            (conditions:vico-bad-index ()
                              (loop-finish)))
                      :finally (setf current-style
                                     (update-style current-style hl:*default-style*))
                               (incf visual-line))
                (handler-case
                    (buf:cursor-next-line top)
                  (conditions:vico-bad-line-number ()
                    (loop-finish)))
            :finally (update-style current-style hl:*default-style*)
                     (tui-draw-window-status window)))
    (ev:log-event (format nil "redisplay of ~a took ~d ms"
                          window
                          (/ (- (get-internal-real-time) b) 1000.0)))))

(defmethod window-x ((window tui-window))
  (term:rect-x (term:dimensions window)))
(defmethod window-y ((window tui-window))
  (term:rect-y (term:dimensions window)))
(defmethod window-width ((window tui-window))
  (term:rect-cols (term:dimensions window)))
(defmethod window-height ((window tui-window))
  (term:rect-rows (term:dimensions window)))

(defun point-column (point)
  (loop :with width = 0
        :with it = (buf:cursor-bol (buf:copy-cursor point))
        :until (buf:cursor= it point)
        :do (incf width (char-display-width (buf:char-at it)))
            (buf:cursor-next-char it)
        :finally (return width)))

(defmethod initialize-instance :after ((window tui-window) &key &allow-other-keys)
  (with-accessors ((top-line window-top-line)
                   (point window-point)
                   (buffer window-buffer)
                   (point-column window-point-column))
      window
    (or top-line (setf top-line (buf:make-cursor buffer 0 :track t :static t)))
    (or point (setf point (buf:make-cursor buffer 0 :track t :track-lineno-p t)))
    (setf point-column (point-column point))))

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

(defmethod scroll-window ((window tui-window) lines)
  (buf:move-cursor-lines* (window-top-line window) lines)
  (tui-clamp-cursor-to-window window)
  (window-point-to-max-column window))

(defmethod (setf window-point-column) (new-value (window tui-window))
  (if new-value
      (setf (slot-value window 'point-column) new-value)
      (setf (slot-value window 'point-column) (point-column (window-point window)))))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :dimensions (term:make-rect :x x :y y
                                                         :cols width :rows height)
                             :buffer buffer
                             ;;:name (buf:buffer-filename buffer)
                             ))
