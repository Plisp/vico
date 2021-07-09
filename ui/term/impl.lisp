;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  terminal implementation of frontend interface
;;

(in-package :vico-term.impl)

(defclass tui (term:tui ui)
  ()
  (:default-initargs :event-handler #'tui-handle-event))

(defclass tui-window (term:standard-window window)
  ((point-column :reader window-point-column)
   (show-line-numbers :initarg :line-numbers
                      :accessor show-line-numbers
                      :type boolean)
   ;; general
   (top-line :initarg :top-line
             :initform nil
             :accessor window-top-line)
   (point :initarg :point
          :initform nil
          :accessor window-point)
   (buffer :initarg :buffer
           :initform nil
           :accessor window-buffer)
   (name :initarg :name
         :initform "a +2 unnamed buffer"
         :accessor window-name)
   (cursor :accessor cursor)
   (floating :initarg :floating
             :accessor floating-p)
   (show-status :initarg :show-status
                :accessor show-status))
  (:documentation "The only type of window"))

;;; tui

;; setf methods are a deliberate omission as they do not really make sense
(defmethod height ((ui tui))
  (term:rows ui))
(defmethod width ((ui tui))
  (term:cols ui))

(defmethod focused-window ((ui tui))
  (term:focused-window ui))
(defmethod (setf focused-window) (new-value (ui tui))
  (setf (term:focused-window ui) new-value))

(defmethod windows ((ui tui))
  (term:windows ui))
(defmethod (setf windows) (new-value (ui tui))
  (setf (term:windows ui) new-value))

(defmethod term:handle-resize progn ((ui tui))
  (log:log :resized-terminal-to (width ui) (height ui))
  ;; needed for now, as window relayouting must happen on command thread
  (ed:queue-command (ed:command-queue ed:*editor*)
                    (list #'redisplay ui)))

(defmethod term:initialize :after ((ui tui))
  (setf (running-p ui) t)
  (push ui (ed:frontends ed:*editor*))
  (term:set-cursor-shape :bar :blink-p t))

(defmethod term:run :around ((ui tui) &key)
  (call-next-method ui :redisplay-on-input nil))

(defmethod start ((ui tui))
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl) (*terminal-io* *standard-output*))
    (term:run ui)
    (deletef (ed:frontends ed:*editor*) ui)))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (term:stop ui))))

(defun tui-handle-event (tui event)
  (let ((canonicalized (etypecase event
                         (character event)
                         (keyword event)
                         (list
                          (let ((name (first event)))
                            (typecase name
                              (character
                               (make-keyword
                                (apply #'concatenate 'string
                                       `(,@(mapcar #'string (cdr event))
                                         "-" ,(string-upcase name)))))
                              (keyword (first event) ; TODO mouse handling?
                               )))))))
    (when (log:log canonicalized)
      (let ((queue (curry #'ed:queue-command (ed:command-queue ed:*editor*))))
        (funcall queue (bindings:lookup-binding tui canonicalized))
        (funcall queue (list #'redisplay tui))))))

(defun tui-draw-window-status (window) ;TODO determine based on buffer type
  ;; per window status
  (let* ((status-line
           (with-output-to-string (status-string)
             (format status-string "~a | " (window-name window))
             (let ((point (window-point window)))
               (format status-string "line: ~d column: ~d"
                       (buf:line-at point) ; TODO use the actual grapheme column
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
    ;; XXX this is a terrible hack to ensure changing geometry doesn't cause ongoing
    ;; TODO redisplays to crash into the debugger. Atomic geometry updates are needed
    (handler-case
        (call-next-method)
      #+sbcl
      (sb-int:invalid-array-index-error (e)
        (log:log :caught-index-error-during-redisplay)
        (log:log e))
      #-sbcl
      (simple-error (e)
        (log:log :caught-error-during-redisplay)
        (log:log e)))))

(defmethod term:redisplay :after ((ui tui))
  (apply #'uncursed-sys::set-cursor-position (cursor (focused-window ui))))

(defun map-windows (fn layout)
  (labels ((rec (layout)
             (if (consp (car layout))
                 (mapcar #'rec layout)
                 layout)))
    (mapcar fn (rec layout))))

;; we just do this using line bars now
;; (defun add-borders (border-window layout &optional (horizontal nil))
;;   (loop :with new = (list)
;;         :for element :in layout
;;         :do (if (proper-list-p element)
;;                 (push (add-borders border-window element (not horizontal)) new)
;;                 (push element new))
;;             (push `(,border-window . 1) new)
;;         :finally (pop new) ; remove final border
;;                  (return (nreverse new))))

(defun layout-windows (ui)
  (map-windows (lambda (layout)
                 (destructuring-bind (window (width . height) (x . y))
                     layout
                   (setf (window-width window) width
                         (window-height window) height
                         (window-x window) x
                         (window-y window) y)))
               (vico-core.ui::calc-layout
                (layout ui)
                (cons (width ui) (height ui)))))

(defmethod redisplay ((ui tui) &key force-p)
  (declare (ignore force-p))
  (log:log "requested-redisplay")
  (when (running-p ui)
    (layout-windows ui)
    (map () #'tui-clamp-window-to-cursor (windows ui))
    (term:wakeup ui)))

;;; window

(defun char-display-width (char)
  (let ((char-width (term:character-width char)))
    (cond ((> char-width -1) (values char-width t)) ; printable
          ((char= char #\tab) 8) ; TODO configurable
          (t
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

(defmethod term:handle-mouse-event ((window tui-window) ui button state line col
                                    &key &allow-other-keys)
  (declare (ignore window ui button state line col))
  )

;; TODO determine if this is even needed
(defmethod term:handle-key-event ((window tui-window) ui key)
  ;;(bindings:lookup-binding ui key window)
  nil)

(defun end-of-visual-line-cursor (c window-width)
  (let ((cursor (buf:copy-cursor c)))
    (handler-case
        (loop :for char = (buf:char-at cursor)
              :for width-traversed = 0
                :then (+ width-traversed
                         (char-display-width char))
              :do (buf:cursor-next-char cursor)
              :until (or (> width-traversed window-width)
                         (char= char #\newline))
              :finally (return cursor))
      (conditions:vico-bad-index ()
        (buf:move-cursor-to cursor (buf:size (buf:cursor-buffer cursor)))))))

(defmethod term:present ((window tui-window))
  "This routine may not modify any window parameters, as it does not run on the main
thread and may race."
  (let ((b (get-internal-real-time))
        (adjustment (list 0 0 0 0))) ; x y width height
    (with-accessors ((height window-height)
                     (width window-width)
                     (buffer window-buffer)
                     (point window-point)
                     (top-line window-top-line)
                     (show-line-numbers show-line-numbers)
                     (cursor cursor))
        window
      ;; border
      (if (floating-p window)
          (with-accessors ((wx window-x)
                           (wy window-y)
                           (ui window-ui))
              window
            (when (or (> (+ wx width) (width ui))
                      (> (+ wy height) (height ui)))
              (setf width (min (- (width ui) wx) width)
                    height (min (- (height ui) wy) height)
                    wx 0 wy 0))
            (loop :for visual-line :from 2 :below height
                  :do (term:put #\│ visual-line 1))
            (loop :for visual-line :from 2 :below height
                  :do (term:put #\│ visual-line width))
            (loop :for visual-column :from 2 :below width
                  :do (term:put #\─ 1 visual-column))
            (loop :for visual-column :from 2 :below width
                  :do (term:put #\─ height visual-column))
            (term:put #\╭ 1 1)
            (term:put #\╰ height 1)
            (term:put #\╮ 1 width)
            (term:put #\╯ height width)
            (setf (first adjustment) 1
                  (second adjustment) 1
                  (third adjustment) -2
                  (fourth adjustment) -2))
          (let* ((border (plusp (window-x window)))
                 (leftbar-width (if show-line-numbers
                                    (+ 2 ; padding
                                       (ceiling (log (+ (buf:line-at top-line)
                                                        (1- height)) ; status line
                                                     10)))
                                    (if border 1 0)))
                 (muted-style (term:make-style :fg #x586e75
                                               :bg hl::*default-bg-color*))
                 (emphasis-style (term:make-style :fg hl::*default-fg-color*
                                                  :bg hl::*default-bg-color*
                                                  :boldp t)))
            ;; line numbers serve as a border
            (if show-line-numbers
                (loop :with c = (buf:copy-cursor top-line)
                      :for visual-line :from 1 :below height ; status
                      :for line = (buf:line-at c)
                      :do (term:puts (princ-to-string line) visual-line 2
                                     (if (= line (buf:line-at point))
                                         emphasis-style
                                         muted-style))
                          (handler-case
                              (buf:cursor-next-line c)
                            (conditions:vico-bad-line-number ()
                              (loop-finish)))) ; reached end of file
                (loop :for visual-line :from 1 :below height ; status
                      :do (term:put #\│ visual-line 1 muted-style)))
            (setf (first adjustment) leftbar-width
                  (third adjustment) (- leftbar-width))))
      (incf (window-x window) (first adjustment))
      (incf (window-y window) (second adjustment))
      (incf width (third adjustment))
      (incf height (fourth adjustment))
      ;; text drawing loop
      (loop
        :initially (or buffer (return))
                   (when (show-status window)
                     (decf visual-end))
        :with top = (buf:cursor-bol (buf:copy-cursor top-line))
        :with visual-end = height
        :with visual-line = 1
        :with current-style = hl:*default-style*
        :until (> visual-line visual-end)
        :do (loop
              :initially (when-let (first (first styles))
                           (setf current-style first))
              :with end = (end-of-visual-line-cursor top width)
              :with spans = (sort (styles-for-window window top end)
                                  #'buf:cursor<
                                  :key #'buf:span-start)
              :with styles = (sort (remove-if-not
                                    #'(lambda (span)
                                        (and (buf:cursor< (buf:span-start span) top)
                                             (buf:cursor> (buf:span-end span) top)))
                                    spans)
                                   #'buf:cursor<
                                   :key #'buf:span-end)
              :with column = 1
              :with char
              :with last-width = 0
              :with display-width
              :with printablep
              :do (when (buf:cursor= top point)
                    (setf cursor
                          (list (+ (window-y window) (1- visual-line))
                                (+ (window-x window) (1- (+ column last-width))))))
                  ;; enabling spans
                  (loop :while ; TODO keep empty style fields
                        (when-let (span (first spans))
                          (when (buf:cursor= top (buf:span-start span))
                            (let ((style (span-style span)))
                              (assert style)
                              (setf current-style style)
                              (pop spans)
                              (push span styles)
                              (setf styles ; TODO need a proper min stack
                                    (sort styles #'buf:cursor< :key #'buf:span-end))))))
                  ;; ending enabled spans - 'styles'
                  (loop :while
                        (when-let (s (first styles))
                          (when (buf:cursor>= top (buf:span-end s))
                            (pop styles)
                            (if-let (prev (first styles))
                              (let ((prev-style (span-style prev)))
                                (assert prev-style)
                                (setf current-style prev-style))
                              (setf current-style hl:*default-style*)))))
                  ;; bounds check as we may get invalidated
                  (multiple-value-setq (display-width printablep)
                    (char-display-width
                     (setf char
                           (handler-case
                               (buf:char-at top)
                             (conditions:vico-bad-index ()
                               (loop-finish))))))

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
                        (#\tab (term:puts "        "
                                          visual-line column
                                          (style-to-term current-style)))
                        (#\newline (loop-finish))
                        (otherwise
                         (let ((code (char-code char)))
                           (if (or (<= 0 code 31) (= code 127))
                               (term:puts (format nil "^~c" ; control char
                                                  (code-char
                                                   (logxor #x40 (char-code char))))
                                          visual-line column
                                          (style-to-term current-style))
                               (term:put #.(code-char #xfffd) ; malformed
                                         visual-line column
                                         (style-to-term current-style)))))))
                  (handler-case
                      (buf:cursor-next-char top)
                    (conditions:vico-bad-index ()
                      (loop-finish)))
              :finally (setf current-style hl:*default-style*)
                       (incf visual-line))
            (handler-case
                (buf:cursor-next-line top)
              (conditions:vico-bad-line-number ()
                (loop-finish))))
      (decf (window-x window) (first adjustment))
      (decf (window-y window) (second adjustment))
      (decf width (third adjustment))
      (decf height (fourth adjustment))
      (when (and buffer (show-status window))
        (tui-draw-window-status window)))
    (log:log (format nil "redisplay of ~a took ~f ms"
                     window
                     (/ (- (get-internal-real-time) b) 1000.0)))))

(defmethod window-x ((window tui-window))
  (term:rect-x (term:dimensions window)))
(defmethod (setf window-x) (new-value (window tui-window))
  (setf (term:rect-x (term:dimensions window)) new-value))

(defmethod window-y ((window tui-window))
  (term:rect-y (term:dimensions window)))
(defmethod (setf window-y) (new-value (window tui-window))
  (setf (term:rect-y (term:dimensions window)) new-value))

(defmethod window-width ((window tui-window))
  (term:rect-cols (term:dimensions window)))
(defmethod (setf window-width) (new-value (window tui-window))
  (setf (term:rect-cols (term:dimensions window)) new-value))

(defmethod window-height ((window tui-window))
  (term:rect-rows (term:dimensions window)))
(defmethod (setf window-height) (new-value (window tui-window))
  (setf (term:rect-rows (term:dimensions window)) new-value))

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
                   (point-column window-point-column)
                   (cursor cursor))
      window
    (when buffer
      (setf cursor (list 0 0))
      (or top-line (setf top-line (buf:make-cursor buffer 0 :track t :static t)))
      (or point (setf point (buf:make-cursor buffer 0 :track t :track-lineno-p t)))
      (setf point-column (point-column point)))))

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

(defmethod make-window ((ui tui) x y width height &key buffer floating
                                                    (line-numbers t) (show-status t))
  (let ((new (make-instance 'tui-window
                            :ui ui
                            :dimensions (term:make-rect :x x :y y
                                                        :cols width :rows height)
                            :buffer buffer
                            :line-numbers line-numbers
                            :floating floating
                            :show-status show-status)))
    (when buffer
      (setf (window-name new) (buf:filename buffer)))
    new))
