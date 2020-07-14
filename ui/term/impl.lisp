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
             (setf original-termios (term:setup-terminal-input))
             (ti:set-terminal (uiop:getenv "TERM"))
             (ti:tputs ti:enter-ca-mode)
             (format t "~c[?1006h~c[?1003h" #\esc #\esc)
             (format t "~c[48;2;0;43;54m" #\escape)
             (ti:tputs ti:clear-screen) ;XXX assuming back-color-erase
             #+sbcl (sb-thread:barrier (:write)) ; is this needed?
             (setf (running-p ui) t)
             (redisplay ui :force-p t)
             (catch 'quit-ui-loop
               (loop
                 (let ((event (term:read-terminal-event)))
                   (queue-event (event-queue *editor*) (tui-parse-event ui event)))))
             (deletef (frontends *editor*) ui))
        (setf (running-p ui) nil)
        #+sbcl (sb-thread:barrier (:write))
        (ti:tputs ti:orig-pair)
        (ti:tputs ti:exit-ca-mode)
        (format t "~c[?1003l~c[?1006l" #\esc #\esc)
        (when original-termios
          (term:restore-terminal-input original-termios))
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
                 (format s "~c[" #\escape)
                 (loop :for (name attr) :on diff :by #'cddr
                       :do (write-string (attr-string name attr) s)))))
        (setf (aref s (1- (length s))) #\m) ; last #\;->#\m
        (write-string s)))))

(defun tui-draw-window-status (window) ;TODO make generic for sure
  (ti:tputs ti:cursor-address (1- (window-height window)) (window-x window))
  (format t "~c[48;2;7;54;66m" #\escape)
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
    (ti:tputs ti:clr-eol)
    (format t "~c[48;2;0;43;54m" #\escape)))

;;TODO don't use the actual cursor, represent as selection
;; selections can be optimized for one character, second cursor static
(defun tui-draw-point (tui)
  (let* ((focused (focused-window tui))
         (point (window-point focused)))
    (ti:tputs ti:cursor-address
              (- (buf:line-at point)
                 (buf:line-at (window-top-line focused)))
              (- (buf:index-at point)
                 (buf:index-at (buf:cursor-bol (buf:copy-cursor point)))))))

(defun tui-redisplay-window (window force-p)
  "This routine may not modify any window parameters, as it does not run on the main
thread and may race."
  ;; (unless (> (get-internal-run-time) (last-redisplayed window))
  ;;   (return-from tui-redisplay-window))
  (let ((window-height (window-height window))
        (window-width (window-width window))
        (delta (car (scroll-delta window))))
    (loop :initially (unless (or force-p ; full redisplay after edits (optimize?)
                                 (/= last-edit-time (last-edit-time window))
                                 (not (zerop delta))
                                 (zerop (buf:size (window-buffer window))))
                       (loop-finish))
          :with last-edit-time = (buf:edit-timestamp (window-buffer window))
          :with top = (buf:cursor-bol (buf:copy-cursor (window-top-line window)))
          :with visual-end =  (1- window-height)
          :with visual-line = 1
          :with current-style = hl:*default-style*
          :until (> visual-line visual-end)
          :do (let* ((line-text
                       (loop
                         :with char-index = 0
                         :with total-width = 0
                         :for char = (buf:char-at top)
                         :for width = (term:character-width char)
                         :while (and (not (char= char #\newline))
                                     (<= (incf total-width width) window-width))
                         :do (handler-case (buf:cursor-next-char top)
                               (conditions:vico-bad-index ()
                                 (loop-finish)))
                             (incf char-index)
                         :finally (return
                                    (let ((enc:*suppress-character-coding-errors* t))
                                      (buf:subseq-at
                                       (buf:cursor-prev-char top char-index)
                                       char-index)))))
                     (syntax (make-array (length line-text) :initial-element :text)))
                ;; (dolist (lexer (stdbuf:lexers buffer));todo remove
                ;;   (funcall lexer syntax line-text))
                (ti:tputs ti:cursor-address (1- visual-line) 0)
                (loop :for c :across line-text
                      :for idx :from 0
                      :do (let ((next-style (hl:syntax-style (svref syntax idx))))
                            (update-style current-style next-style)
                            (setf current-style next-style))
                          (write-string (nth-value 1 (term:character-width c))))
                (ti:tputs ti:clr-eol))
              (incf visual-line)
              (handler-case
                  (buf:cursor-next-line top)
                (conditions:vico-bad-line-number ()
                  (loop :until (> visual-line visual-end)
                        :do (ti:tputs ti:cursor-address (1- visual-line) 0)
                            (write-char #\~)
                            (ti:tputs ti:clr-eol)
                            (incf visual-line))
                  (loop-finish)))
          :finally (update-style current-style hl:*default-style*)
                   (setf (last-edit-time window) last-edit-time
                         ;;(last-redisplayed window) (get-internal-run-time)
                         )
                   (atomics:atomic-decf (car (scroll-delta window)) delta)
                   (tui-draw-window-status window))))

(defun tui-redisplay (tui &key force-p)
  (handler-case
      (progn
        (ti:tputs ti:cursor-invisible)
        (dolist (window (windows tui))
          (tui-redisplay-window window force-p))
        (ti:tputs ti:cursor-visible)
        (tui-draw-point tui))
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
  ((last-redisplayed :initform 0 :accessor last-redisplayed)
   (scroll-delta :initform (cons 0 nil) :reader scroll-delta) ;for atomics
   (last-edit-time :initform 0 :accessor last-edit-time)
   ;; general
   (top-line :initarg :top-line
             :initform nil
             :reader window-top-line)
   (point :initarg :point
          :initform nil
          :reader window-point)
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
  (with-slots (top-line point) window
    (or top-line
        (setf top-line (buf:make-cursor (window-buffer window) 0 :track t :static t)))
    (or point
        (setf point (buf:make-cursor (window-buffer window) 0 :track t)))))

;; TODO figure out how to do this better
(defun tui-clamp-window-to-cursor (window)
  (let ((point (window-point window)))
    (or (let ((delta (- (buf:line-at point)
                        (+ (buf:line-at (window-top-line window))
                           (- (window-height window) 2)))))
          (when (plusp delta)
            (tui-scroll-window window delta)))

        (let ((delta (- (buf:line-at (window-top-line window))
                        (buf:line-at point))))
          (when (plusp delta)
            (tui-scroll-window window (- delta)))))))

(defun tui-clamp-cursor-to-window (window)
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

(defun tui-scroll-window (window lines)
  (let ((scrolled (nth-value 1 (buf:move-cursor-lines* (window-top-line window) lines))))
    (atomics:atomic-incf (car (scroll-delta window)) scrolled)))

(defmethod scroll-window ((window tui-window) lines)
  (tui-scroll-window window lines)
  (tui-clamp-cursor-to-window window)
  (redisplay (window-ui window)))

(defun tui-move-point (window count)
  (buf:move-cursor-chars* (window-point window) count))

(defmethod move-point ((window tui-window) &optional (count 1))
  (tui-move-point window count)
  (redisplay (window-ui window)))

;; TODO record column, use display width
(defun tui-move-point-lines (window count)
  (let ((point (window-point window)))
    (buf:move-cursor-lines* point count)
    ))

(defmethod move-point-lines ((window tui-window) &optional (count 1))
  (tui-move-point-lines window count)
  (redisplay (window-ui window)))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer
                             ;;:name (buf:buffer-filename buffer)
                             ))
