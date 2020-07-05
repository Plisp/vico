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
           (progn ;TODO bug? initial redisplay not under altscreen
             (setf original-termios (term:setup-terminal-input))
             (ti:set-terminal (uiop:getenv "TERM"))
             (ti:tputs ti:enter-ca-mode)
             (format t "~c[48;2;0;43;54m" #\escape)
             (ti:tputs ti:clear-screen) ;XXX assuming back-color-erase
             (redisplay ui :force-p t)
             (catch 'quit-ui-loop
               (loop
                 (let ((event (term:read-terminal-event)))
                   (queue-event (event-queue *editor*) (tui-parse-event ui event)))))
             (deletef (frontends *editor*) ui))
        (ti:tputs ti:orig-pair)
        (ti:tputs ti:exit-ca-mode)
        (force-output)
        (when original-termios
          (term:restore-terminal-input original-termios))))))

(defmethod quit ((ui tui))
  (bt:interrupt-thread (ui-thread ui) (lambda () (throw 'quit-ui-loop nil))))

(defun tui-parse-event (tui event)
  (cond ((characterp event)
         (make-key-event :name (case event
                                 (#\rubout :backspace) ;TODO kludge
                                 (#\backspace :backspace)
                                 (#\page :control-l)
                                 (#\eot :control-d)
                                 (#\etx :control-c)
                                 (#\so :control-n)
                                 (#\dle :control-p)
                                 (#\ack :control-f)
                                 (#\stx :control-b)
                                 (#\enq :control-e)
                                 (#\em :control-y)
                                 (#\etb :control-w)
                                 (otherwise event))
                         :window (focused-window tui)))
        ((keywordp event)
         (case event ;XXX
           (:up (make-key-event :name :control-p :window (focused-window tui)))
           (:down (make-key-event :name :control-n :window (focused-window tui)))
           (:left (make-key-event :name :control-b :window (focused-window tui)))
           (:right (make-key-event :name :control-f :window (focused-window tui)))
           (:page-up (make-key-event :name :page-up :window (focused-window tui)))
           (:page-down (make-key-event :name :page-down :window (focused-window tui)))))
        ((and (listp event) (search "SCROLL" (string (first event))))
         (case (first event)
           (:scroll-up (make-key-event :name :control-y :window (focused-window tui)))
           (:scroll-down (make-key-event :name :control-e :window (focused-window tui)))))
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

(defun tui-draw-window-status (window redisplay-start-time) ;TODO make generic for sure
  (ti:tputs ti:cursor-address (1- (window-height window)) (window-x window))
  (format t "~c[48;2;7;54;66m" #\escape)
  ;; per window status
  (let ((status-line
          (with-output-to-string (status-string)
            (format status-string " - ")
            (format status-string "~a | " (window-name window))
            ;; don't display this by default (in large fragged files), not efficient
            (format status-string "cursor at line ~d | "
                    (buf:line-at (window-point window)))
            (format status-string "redisplayed in ~5f secs | "
                    (/ (- (get-internal-run-time) redisplay-start-time)
                       internal-time-units-per-second))
            (format status-string "C-l (re)draws screen, ~
                                   C-e/y scrolls viewport, ~
                                   C-w quits, ~
                                   page-up/down, ~
                                   C-d/backspace, ~
                                   arrow keys as expected"
                    ))))
    (write-string ; XXX assuming ascii
     (subseq status-line 0 (min (length status-line) (window-width window))))
    ;;XXX assuming back-color-erase
    (ti:tputs ti:clr-eol)
    (format t "~c[48;2;0;43;54m" #\escape)))

;;TODO save position in loop
(defun tui-draw-point (tui)
  (let* ((focused (focused-window tui))
         (point (window-point focused)))
    (ti:tputs ti:cursor-address
              (- (buf:line-at point)
                 (buf:line-at (window-top-line focused)))
              (- (buf:index-at point)
                 (buf:index-at (buf:cursor-bol (buf:copy-cursor point)))))))

(defun tui-redisplay-window (window force-p)
  (let ((start-time (get-internal-run-time))
        (window-height (window-height window))
        (window-width (window-width window))
        (delta (car (scroll-delta window))))
    (unless (zerop (buf:size (window-buffer window)))
      (ti:tputs ti:change-scroll-region
                (1- (window-y window))
                (- window-height 2))
      (let* ((buffer (window-buffer window))
             (last-edit-time (buf:edit-timestamp buffer))
             (top (buf:cursor-bol (buf:copy-cursor (window-top-line window))))
             (visual-line 1)
             (visual-end (1- window-height)))
        (loop :initially (unless (or force-p ; full redisplay after edits (optimize?)
                                     (/= last-edit-time (last-edit-time window))
                                     (not (zerop delta)))
                           (return))
                         (handler-case
                             (buf:cursor-next-line top (1- visual-line))
                           (conditions:vico-bad-line-number ()
                             (loop-finish)))
              :with current-style = hl:*default-style*
              :until (> visual-line visual-end)
              :do (let* ((line-text
                           (handler-case
                               (loop
                                 :with char-index = 0
                                 :with total-width = 0
                                 :for char = (buf:char-at top)
                                 :for width = (term:character-width char)
                                 :while (and (not (char= char #\newline))
                                             (<= (incf total-width width) window-width))
                                 :do (buf:cursor-next-char top)
                                     (incf char-index)
                                 :finally (return
                                            (buf:subseq-at
                                             (buf:cursor-prev-char top char-index)
                                             char-index)))
                             (conditions:vico-bad-index ()
                               (loop-finish))))
                         (syntax (make-array (length line-text) :initial-element :text)))
                    ;; (dolist (lexer (stdbuf:lexers buffer));TODO remove
                    ;;   (funcall lexer syntax line-text))
                    (ti:tputs ti:cursor-address (1- visual-line) 0)
                    (ti:tputs ti:clr-eol)
                    (loop :for c :across line-text
                          :for idx :from 0
                          :do (let ((next-style (hl:syntax-style (svref syntax idx))))
                                (update-style current-style next-style)
                                (setf current-style next-style))
                              (write-string (nth-value 1 (term:character-width c)))))
                  (incf visual-line)
                  (handler-case
                      (buf:cursor-next-line top)
                    (conditions:vico-bad-line-number ()
                      (loop-finish)))
              :finally (update-style current-style hl:*default-style*)
                       (setf (last-edit-time window) last-edit-time)
                       (atomics:atomic-decf (car (scroll-delta window)) delta)
                       (loop :until (> visual-line visual-end)
                             :do (ti:tputs ti:cursor-address (1- visual-line) 0)
                                 (write-char #\~)
                                 (ti:tputs ti:clr-eol)
                                 (incf visual-line))
                       (tui-draw-window-status window start-time))))))

;; TODO no scrolling (except on focused window) on terminals without margin support
(declaim (notinline tui-redisplay))
(defun tui-redisplay (tui &key force-p)
  (dolist (window (windows tui))
    (handler-case
        (tui-redisplay-window window force-p)
      (conditions:vico-cursor-invalid ()
        (return-from tui-redisplay))))
  (tui-draw-point tui)
  (force-output)
  t)

(defmethod redisplay ((ui tui) &key force-p)
  (when (ui-thread ui)
    (bt:interrupt-thread (ui-thread ui) (lambda ()
                                          (concurrency:without-interrupts
                                            (tui-redisplay ui :force-p force-p))))))

(defclass tui-window (window)
  ((scroll-delta :initform (cons 0 nil) :reader scroll-delta) ;for atomics
   (last-edit-time :initform 0 :accessor last-edit-time)
   ;; general
   (top-line :initform (error "TOP-LINE cursor required") ;3821
             :initarg :top-line
             :reader window-top-line)
   (point :initarg :point
          :initform nil
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
         :initform "an unnamed buffer"
         :accessor window-name))
  (:documentation "The only type of window"))

(defun tui-clamp-window-to-cursor (window)
  (let ((point (window-point window)))
    (or (let ((delta (- (buf:line-at point) ;cursor-line-difference
                        (+ (buf:line-at (window-top-line window))
                           (- (window-height window) 2)))))
          (when (plusp delta)
            (tui-scroll-window window delta)))

        (let ((delta (- (buf:line-at (window-top-line window)) ;cursor-line-difference
                        (buf:line-at point))))
          (when (plusp delta)
            (tui-scroll-window window (- delta)))))))

(defun tui-clamp-cursor-to-window (window)
  (let ((point (window-point window)))
    (or (let ((delta (- (buf:line-at point)
                        (+ (buf:line-at (window-top-line window)) ;cursor-line-difference
                           (- (window-height window) 2)))))
          (when (plusp delta)
            (move-point-lines window (- delta))))

        (let ((delta (- (buf:line-at (window-top-line window)) ;cursor-line-difference
                        (buf:line-at point))))
          (when (plusp delta)
            (move-point-lines window delta))))))

(defun tui-scroll-window (window lines)
  (let* ((top-line (buf:line-at (window-top-line window)))
         (clamped-lines (if (plusp lines) ;use errors or something
                            (min lines (- (buf:line-count (window-buffer window)) top-line))
                            (max lines (- 1 top-line)))))
    (buf:move-cursor-lines (window-top-line window) clamped-lines)
    (atomics:atomic-incf (car (scroll-delta window)) clamped-lines)))

(defmethod scroll-window ((window tui-window) lines)
  (tui-scroll-window window lines)
  (tui-clamp-cursor-to-window window)
  (redisplay (window-ui window)))

(defun tui-move-point (window count)
  (let ((cursor (window-point window)))
    (if (plusp count)
        (loop :repeat count ;end-of-buffer-p
              :until (= (buf:index-at cursor) (1- (buf:size (window-buffer window))))
              :do (buf:cursor-next-char cursor))
        (loop :repeat (- count)
              :until (zerop (buf:index-at cursor)) ;start-of-buffer-p
              :do (buf:cursor-prev-char cursor)))))

(defmethod move-point ((window tui-window) &optional count)
  (tui-move-point window count)
  (tui-clamp-window-to-cursor window)
  (redisplay (window-ui window)))

(defun tui-move-point-lines (window count)
  (let* ((point (window-point window))
         (columns (- (buf:index-at point) ;cursor-difference
                     (buf:index-at (buf:cursor-bol (buf:copy-cursor point)))))
         (buffer (window-buffer window)))
    (if (plusp count)
        (loop :repeat count
              :until (= (buf:line-at point) (buf:line-count buffer)) ;last-line-p
              :do (buf:cursor-next-line point)
              :finally (unless (= (buf:line-at point) (buf:line-count buffer)) ;last-line-p
                         (loop :repeat columns ;end-of-buffer-p
                               :while (and (< (buf:index-at point) (1- (buf:size buffer)))
                                           (char/= (buf:char-at point) #\newline))
                               :do (tui-move-point window 1))))
        (loop :repeat (- count)
              :until (= (buf:line-at point) 1) ;first-line-p
              :do (buf:cursor-prev-line point)
              :finally (tui-clamp-window-to-cursor window)
                       (unless (= (buf:line-at point) 1) ;first-line-p
                         (loop :repeat columns ;end-of-buffer-p
                               :while (and (< (buf:index-at point) (1- (buf:size buffer)))
                                           (char/= (buf:char-at point) #\newline))
                               :do (tui-move-point window 1)))))))

(defmethod move-point-lines ((window tui-window) &optional count)
  (tui-move-point-lines window count)
  (tui-clamp-window-to-cursor window)
  (redisplay (window-ui window)))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer
                             :top-line (buf:track-cursor (buf:make-cursor buffer 0))
                             :point (buf:track-cursor (buf:make-cursor buffer 0))
                             ;;name (buf:buffer-name buffer)
                             ))
