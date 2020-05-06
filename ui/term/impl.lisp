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
          (let ((new-width (truncate (* (window-width window) xscale)))
                (new-height (truncate (* (window-height window) yscale))))
            (setf (slot-value window 'width) new-width
                  (slot-value window 'height) new-height
                  (lines window) (adjust-array (lines window) (1- new-height)))))
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
           (progn ;TODO bug? initial redisplay not under altscreen
             (setf original-termios (term:setup-terminal-input))
             (ti:set-terminal (uiop:getenv "TERM"))
             (ti:tputs ti:enter-ca-mode)
             (format t "~C[48;2;0;43;54m" #\escape)
             (ti:tputs ti:clear-screen) ;XXX assuming back-color-erase
             (%tui-redisplay ui :force-p t)
             (catch 'quit-ui-loop ;XXX use sighandler
               (setf original-handler (c-signal +sigwinch+ (cffi:callback sigwinch-handler)))
               (loop
                 (queue-event
                  (event-queue *editor*)
                  (let ((event (term:read-terminal-event)))
                    (%tui-parse-event ui event)))))
             (deletef (frontends *editor*) ui))
        (ti:tputs ti:exit-ca-mode)
        (finish-output)
        (when original-termios (term:restore-terminal-input original-termios))
        (when original-handler (c-signal +sigwinch+ original-handler))))))

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
        (setf (aref s (1- (buf:length s))) #\m) ; last #\;->#\m
        (write-string s)))))

(defun %tui-draw-window-status (window redisplay-start-time) ;TODO make generic for sure
  (ti:tputs ti:cursor-address (1- (window-height window)) (1- (window-y window)))
  (format t "~C[48;2;7;54;66m" #\escape)
  ;; per window status
  (let ((status-line
          (with-output-to-string (status-string)
            (format status-string " - ")
            (format status-string "~A | " (window-name window))
            (format status-string "cursor at line ~d | " (buf:line-at (window-point window)))
            (format status-string "redisplayed in ~5f secs | "
                    (/ (- (get-internal-run-time) redisplay-start-time)
                       internal-time-units-per-second))
            (format status-string "C-l (re)draws screen, C-e/y scrolls window, C-c quits ,~
                                   up/down arrows = pageup/down, C-d/backspace as expected"
                    ))))
    (write-string ; XXX assuming ascii
     (subseq status-line 0 (min (buf:length status-line) (window-width window))))
    ;;XXX assuming back-color-erase
    (ti:tputs ti:clr-eol)
    (format t "~C[48;2;0;43;54m" #\escape)))

(defun %tui-draw-point (tui)
  (let* ((focused (focused-window tui))
         (cursor (window-point focused))
         (column (- (buf:index-at cursor) ;XXX variable width
                    (buf:index-at (buf:cursor-to-line-start (buf:copy-cursor cursor))))))
    (ti:tputs ti:cursor-address
              (- (buf:line-at cursor) (buf:line-at (window-top-line focused)))
              column)))

(defun %tui-redisplay (tui &key force-p)
  (let ((start-time (get-internal-run-time)))
    (dolist (window (windows tui))
      (unless (zerop (buf:length (window-buffer window)))
        (ti:tputs ti:change-scroll-region
                  (1- (window-y window))
                  (- (window-height window) 2))
        (let* ((buffer (window-buffer window))
               (last-top (last-top-line window))
               (top (buf:cursor-to-line-start (window-top-line window)))
               (top-line (buf:line-at top))
               (delta (- top-line (buf:line-at last-top)))
               no-change)
          (cond ((or force-p (>= (abs delta) (window-height window)))
                 (ti:tputs ti:clear-screen))
                ((and (= (stdbuf:last-edit-time buffer) (last-edit-time window))
                      (zerop delta))
                 (setf no-change t))
                ((plusp delta)
                 (ti:tputs ti:parm-index delta))
                ((minusp delta)
                 (ti:tputs ti:parm-rindex (- delta))))
          ;; (unless force-p
          ;;   (format t "~C[48;2;100;20;40m" #\escape))
          (loop :initially (when no-change (loop-finish))
                :with visual-line = 1
                :with window-height = (window-height window)
                :with current-style = hl:*default-style*
                :until (or (> visual-line (1- window-height))
                           (> (buf:line-at top) (buf:line-count buffer)))
                :do (let* ((start-of-line (buf:copy-cursor top))
                           (line-text
                             (loop :with total-width = 0
                                   :for char = (buf:char-at top)
                                   :for width = (term:character-width char)
                                   :while (and (not (char= char #\newline))
                                               (<= (incf total-width width)
                                                   (window-width window)))
                                   :do (buf:cursor-next top)
                                   :finally (return
                                              (buf:subseq-at start-of-line
                                                             (- (buf:index-at top)
                                                                (buf:index-at start-of-line))))))
                           (syntax (make-array (buf:length line-text) :initial-element :text)))
                      (unless (and (string= (aref (lines window) (1- visual-line))
                                            line-text)
                                   (not force-p))
                        ;;TODO may change w/ multiline highlighting
                        (dolist (lexer (stdbuf:lexers buffer))
                          (funcall lexer buffer syntax line-text
                                   (buf:index-at start-of-line)
                                   (+ (buf:index-at start-of-line) (buf:length line-text))))
                        (ti:tputs ti:cursor-address (1- visual-line) 0)
                        (ti:tputs ti:clr-eol)
                        (setf (aref (lines window) (1- visual-line)) line-text)
                        (loop :for c :across line-text
                              :for idx :from 0
                              :do (let ((next-style (hl:syntax-style (svref syntax idx))))
                                    (update-style current-style next-style)
                                    (setf current-style next-style))
                                  (write-string (nth-value 1 (term:character-width c))))))
                    (incf visual-line)
                    (buf:cursor-next-line top)
                :finally (update-style current-style hl:*default-style*)
                         (buf:cursor-move-to-line last-top top-line)
                         (buf:cursor-move-to-line top top-line)
                         (%tui-draw-window-status window start-time)
                         (setf (last-edit-time window) (stdbuf:last-edit-time buffer))))))
    (%tui-draw-point tui)
    (force-output))
  nil)

(defmethod redisplay ((ui tui) &key force-p)
  (bt:interrupt-thread (ui-thread ui) (lambda () (%tui-redisplay ui :force-p force-p))))

(defmethod execute ((ui tui) function)
  (bt:interrupt-thread (ui-thread ui) function))

;; TODO must move %top-line using a cursor - may end up anywhere after deletion
;; and search backwards for line start

(defclass tui-window (window)
  ((last-top-line :initform (error "LAST-TOP-LINE cursor required")
                  :initarg :last-top-line
                  :reader last-top-line)
   (top-line :initform (error "TOP-LINE cursor required") ;3821
             :initarg :top-line
             :reader window-top-line)
   (lines :initarg :lines
          :accessor lines
          :type (simple-array string))
   (last-edit-time :initform (get-internal-real-time)
                   :accessor last-edit-time)
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
           :accessor window-buffer
           :type stdbuf:standard-buffer)
   (name :initarg :name
         :accessor window-name))
  (:documentation "The only type of window"))

(defmethod (setf height) :after (new-value (window tui-window))
  (setf (lines window) (adjust-array (lines window) (1- new-value))))

(defun %tui-clamp-window-to-cursor (window)
  (let ((point (window-point window)))
    (or (let ((delta (- (buf:line-at point)
                        (+ (buf:line-at (window-top-line window))
                           (- (window-height window) 2)))))
          (when (plusp delta)
            (%tui-scroll-window window delta)))

        (let ((delta (- (buf:line-at (window-top-line window))
                        (buf:line-at point))))
          (when (plusp delta)
            (%tui-scroll-window window (- delta)))))))

(defun %tui-clamp-cursor-to-window (window)
  (let ((point (window-point window)))
    (or (let ((delta (- (buf:line-at point)
                        (+ (buf:line-at (window-top-line window))
                           (- (window-height window) 2)))))
          (when (plusp delta)
            (move-point-lines window (- delta))))

        (let ((delta (- (buf:line-at (window-top-line window))
                        (buf:line-at point))))
          (when (plusp delta)
            (move-point-lines window delta))))))

(defun %tui-scroll-window (window lines)
  (let* ((top-line (buf:line-at (window-top-line window)))
         (clamped-lines (if (plusp lines)
                            (min lines (- (buf:line-count (window-buffer window)) top-line))
                            (max lines (- 1 top-line)))))
    (buf:cursor-move-line (window-top-line window) clamped-lines)))

(defmethod scroll-window ((window tui-window) lines)
  (bt:interrupt-thread (ui-thread (window-ui window))
                       (lambda ()
                         (%tui-scroll-window window lines)
                         (%tui-clamp-cursor-to-window window)
                         (redisplay (window-ui window)))))

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
  (bt:interrupt-thread (ui-thread (window-ui window))
                       (lambda ()
                         (%tui-move-point window count)
                         (%tui-clamp-window-to-cursor window)
                         (redisplay (window-ui window)))))

(defun %tui-move-point-lines (window count)
  (let* ((point (window-point window))
         (columns (- (buf:index-at point) ;XXX variable width
                     (buf:index-at (buf:cursor-to-line-start (buf:copy-cursor point)))))
         (buffer (window-buffer window)))
    (if (plusp count)
        (unless (= (buf:line-at point) (buf:line-count buffer))
          (loop :repeat count
                :until (= (buf:line-at point) (buf:line-count buffer))
                :do (buf:cursor-next-line point)
                :finally
                   (loop :repeat columns
                         :while (and (< (buf:index-at point) (buf:length buffer))
                                     (char/= (buf:char-at point) #\newline))
                         :do (%tui-move-point window 1))))
        (unless (= (buf:line-at point) 1)
          (loop :repeat (- count)
                :until (= (buf:line-at point) 1)
                :do (buf:cursor-prev-line point)
                :finally (%tui-clamp-window-to-cursor window)
                         (loop :repeat columns
                               :while (and (< (buf:index-at point) (buf:length buffer))
                                           (char/= (buf:char-at point) #\newline))
                               :do (%tui-move-point window 1)))))))

(defmethod move-point-lines ((window tui-window) &optional count)
  (bt:interrupt-thread (ui-thread (window-ui window))
                       (lambda ()
                         (%tui-move-point-lines window count)
                         (%tui-clamp-window-to-cursor window)
                         (redisplay (window-ui window)))))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer
                             :last-top-line (buf:make-cursor buffer 0)
                             :top-line (buf:make-cursor buffer 0)
                             :lines (make-array (1- height) :element-type 'string
                                                            :initial-element "")
                             :point (buf:make-cursor buffer 0)
                             :name (or (buf:buffer-name buffer) "an unnamed buffer")))
