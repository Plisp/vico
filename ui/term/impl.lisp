;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defmethod start ((ui tui))
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl) (*terminal-io* *standard-output*))
    (push ui (frontends *editor*))
    (let (original-termios)
      (unwind-protect
           (progn
             (setf original-termios (tui:setup-terminal 0))
             (ti:set-terminal (uiop:getenv "TERM"))
             (tui:enable-alternate-screen)
             (format t "~c[?1006h~c[?1003h" #\esc #\esc)
             (format t "~c[48;2;0;43;54m" #\esc)
             (tui:clear-screen) ;TODO assuming bce
             (setf (running-p ui) t)
             (redisplay ui :force-p t)
             (catch 'quit-ui-loop
               (loop
                 (let ((event (tui:read-event)))
                   (queue-event (event-queue *editor*) (tui-parse-event ui event)))))
             (deletef (frontends *editor*) ui))
        (setf (running-p ui) nil)
        (tui:disable-alternate-screen)
        (format t "~c[?1003l~c[?1006l" #\esc #\esc)
        (when original-termios
          (tui:restore-terminal original-termios 0))
        (force-output)))))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (throw 'quit-ui-loop nil))))

;; XXX kludge
(defun tui-parse-event (tui event)
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
           (:up (make-key-event :name :control-p :window (focused-window tui)))
           (:down (make-key-event :name :control-n :window (focused-window tui)))
           (:left (make-key-event :name :control-b :window (focused-window tui)))
           (:right (make-key-event :name :control-f :window (focused-window tui)))
           (:page-up (make-key-event :name :page-up :window (focused-window tui)))
           (:page-down (make-key-event :name :page-down :window (focused-window tui)))))
        ((and (listp event) (search "SCROLL" (string (first event))))
         (log-event event)
         (case (first event)
           (:scroll-up (make-key-event :name :control-y :window (focused-window tui)))
           (:scroll-down (make-key-event :name :control-t :window (focused-window tui)))))
        ((and (listp event) (search "META" (string (first event))))
         (log-event event)
         (case (cdr event)
           (#\b (make-key-event :name :alt-b :window (focused-window tui)))
           (#\f (make-key-event :name :alt-f :window (focused-window tui)))))
        (t
         (make-key-event :name :null :window (focused-window tui)))))

;; TODO implement window abstraction with borders
;; TODO portable terminal code

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
  (tui:set-cursor-position (1- (window-height window)) (window-x window))
  (format t "~c[48;2;7;54;66m" #\esc)
  ;; per window status
  (let ((status-line
          (with-output-to-string (status-string)
            (format status-string " - ")
            (format status-string "~a | " (window-name window))
            ;; don't display this by default (in large fragged files), not efficient
            (format status-string "line ~d/~d | "
                    (buf:line-at (window-point window))
                    (buf:line-count (window-buffer window)))
            (format status-string "C-l (re)draws screen, ~
                                   mousewheel scrolls viewport, ~
                                   C-w quits, ~
                                   some emacs bindings"
                    )
            ;;(format status-string " | index ~d" (buf:index-at (window-point window)))
            )))
    (write-string ; XXX assuming ascii
     (subseq status-line 0 (min (length status-line) (window-width window))))
    ;;XXX assuming back-color-erase
    (tui:clear-to-end-of-line)
    (format t "~c[48;2;0;43;54m" #\esc)))

;;TODO don't use the actual cursor, represent as selection
;; selections can be optimized for one character, second cursor static

;; some function list will exist for obtaining style spans (cursor,cursor,style)
;; a grid provided by UNCURSED will be populated in order
;;

(defun tui-draw-window-point (window)
  (tui:set-cursor-position (- (buf:line-at (window-point window))
                              (buf:line-at (window-top-line window)))
                           (- (buf:index-at (window-point window))
                              (buf:index-at (buf:cursor-bol (buf:copy-cursor
                                                             (window-point window)))))))

(defun tui-redisplay-window (window force-p)
  "This routine may not modify any window parameters, as it does not run on the main
thread and may race."
  (with-accessors ((height window-height)
                   (width window-width)
                   (buffer window-buffer)
                   (point window-point)
                   (top-line window-top-line)
                   (last-top-line last-top-line)
                   (last-edit-time last-edit-time))
      window
    (loop :initially (when (and (not force-p)
                                (= buffer-edit-time last-edit-time)
                                (buf:cursor= orig-top last-top-line))
                       (loop-finish))
          :with buffer-edit-time = (buf:edit-timestamp buffer)
          :with orig-top = (buf:copy-cursor top-line)
          :with top = (buf:cursor-bol (buf:copy-cursor orig-top))
          :with visual-end =  (1- height)
          :with visual-line = 1
          :with current-style = hl:*default-style*
          :until (> visual-line visual-end)
          :do (loop :initially (tui:set-cursor-position (1- visual-line) 0)
                    :with char-index = 0
                    :with total-width = 0
                    :for char = (buf:char-at top)
                    :for char-width = (tui:character-width char)
                    :while (and (not (char= char #\newline))
                                (<= (incf total-width char-width) (1+ width)))
                    :do (handler-case
                            (buf:cursor-next-char top)
                          (conditions:vico-bad-index ()
                            (loop-finish)))
                        (write-char char)
                        (incf char-index)
                    :finally (tui:clear-to-end-of-line))
              (incf visual-line)
              (handler-case
                  (buf:cursor-next-line top)
                (conditions:vico-bad-line-number ()
                  (loop :until (> visual-line visual-end)
                        :do (tui:set-cursor-position (1- visual-line) 0)
                            (write-char #\~)
                            (tui:clear-to-end-of-line)
                            (incf visual-line))
                  (loop-finish)))
          :finally (update-style current-style hl:*default-style*)
                   (setf last-edit-time buffer-edit-time
                         last-top-line orig-top)
                   (tui-draw-window-status window)
                   (tui-draw-window-point window))))

(defun tui-redisplay (tui &key force-p)
  (handler-case
      (dolist (window (windows tui))
        (tui-redisplay-window window force-p))
    (conditions:vico-cursor-invalid ()
      (return-from tui-redisplay)))
  (force-output)
  t)

(defmethod redisplay ((ui tui) &key force-p)
  (map () #'tui-clamp-window-to-cursor (windows ui))
  (when (running-p ui)
    (bt:interrupt-thread (ui-thread ui) (lambda ()
                                          (concurrency:without-interrupts
                                            (tui-redisplay ui :force-p force-p))))))

(defclass tui-window (window)
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
   (x :initarg :x
      :accessor window-x)
   (y :initarg :y
      :accessor window-y)
   (width :initarg :width
          :accessor window-width)
   (height :initarg :height
           :accessor window-height)
   (buffer :initarg :buffer
           :initform (error "window buffer not provided")
           :accessor window-buffer)
   (name :initarg :name
         :initform "a +2 unnamed buffer"
         :accessor window-name))
  (:documentation "The only type of window"))

(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
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

(defmethod scroll-window ((window tui-window) lines)
  (buf:move-cursor-lines* (window-top-line window) lines)
  (tui-clamp-cursor-to-window window))

;; TODO grapheme clusters
(defmethod move-point ((window tui-window) &optional (count 1))
  (with-accessors ((point window-point))
      window
    (buf:move-cursor-chars* point count)
    (setf (max-point-col window)
          (buf:cursor- point (buf:cursor-bol (buf:copy-cursor point))))))

;; can we handle this?: ཧྐྵྨླྺྼྻྂ
;; TODO record column, use display width
(defmethod move-point-lines ((window tui-window) &optional (count 1))
  (with-accessors ((point window-point)
                   (max-point-col max-point-col)
                   (buffer window-buffer))
      window
    (buf:move-cursor-lines* point count)
    (loop :with max-col = max-point-col
          :until (or (zerop max-col) (char= (buf:char-at point) #\newline)
                     (= (buf:index-at point) (buf:size buffer)))
          :do (buf:cursor-next point)
              (decf max-col))))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer
                             ;;:name (buf:buffer-filename buffer)
                             ))
