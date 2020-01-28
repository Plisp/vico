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
        (setf (width ui) (cdr dimensions) (height ui) (car dimensions))
        (dolist (window (windows ui)) ;XXX boundary windows should be larger to fit term
          (setf (window-width window) (truncate (* (window-width window) xscale))
                (window-height window) (truncate (* (window-height window) yscale))))
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
         (case event
           (#\Page (make-key-event :name :control-l
                                   :window (focused-window tui)))
           (#\Etx (make-key-event :name :control-c
                                  :window (focused-window tui)))
           (#\Enq (make-key-event :name :control-e
                                  :window (focused-window tui)))
           (#\Em (make-key-event :name :control-y
                                 :window (focused-window tui)))))
        ((keywordp event)
         (case event ;XXX
           (:up (make-key-event :name :page-up
                                :window (focused-window tui)))
           (:down (make-key-event :name :page-down
                                  :window (focused-window tui)))))
        ((listp event))
        (t)))

(defparameter *cl-macro-regex* (ppcre-custom:create-scanner "[(](lambda|or|setf|assert|call-method|case|ccase|check-type|cond|ctypecase|decf|declaim|defclass|defconstant|defgeneric|define-compiler-macro|define-condition|define-method-combination|define-modify-macro|define-setf-expander|define-symbol-macro|defmacro|defmethod|defpackage|defparameter|defsetf|defstruct|deftype|defun|defvar|destructuring-bind|do|do-all-symbols|do-external-symbols|do-symbols|do\\*|dolist|dotimes|ecase|etypecase|formatter|handler-bind|handler-case|ignore-errors|in-package|incf|loop|loop-finish|make-method|multiple-value-bind|multiple-value-list|multiple-value-setq|nth-value|otherwise|pop|pprint-exit-if-list-exhausted|pprint-logical-block|pprint-pop|print-unreadable-object|prog|prog1|prog2|prog\\*|psetf|psetq|push|pushnew|remf|restart-bind|restart-case|return|rotatef|shiftf|step|time|trace|typecase|unless|untrace|when|with-[^\\s]*)[)\\s]"))

(defparameter *cl-special-operator-regex* (ppcre-custom:create-scanner "[(](function|block|catch|eval-when|flet|go|if|labels|let|let\\*|load-time-value|locally|macrolet|multiple-value-call|multiple-value-prog1|progn|progv|quote|return-from|setq|symbol-macrolet|tagbody|the|throw|unwind-protect)[)\\s]"))

(defparameter *cl-global-regex* (ppcre-custom:create-scanner "['(\\s](\\*.*\\*|array-dimension-limit|array-rank-limit|array-total-size-limit|boole-1|boole-2|boole-and|boole-andc1|boole-andc2|boole-c1|boole-c2|boole-clr|boole-eqv|boole-ior|boole-nand|boole-nor|boole-orc1|boole-orc2|boole-set|boole-xor|call-arguments-limit|char-code-limit|double-float-epsilon|double-float-negative-epsilon|internal-time-units-per-second|lambda-list-keywords|lambda-parameters-limit|least-negative-double-float|least-negative-long-float|least-negative-normalized-double-float|least-negative-normalized-long-float|least-negative-normalized-short-float|least-negative-normalized-single-float|least-negative-short-float|least-negative-single-float|least-positive-double-float|least-positive-long-float|least-positive-normalized-double-float|least-positive-normalized-long-float|least-positive-normalized-short-float|least-positive-normalized-single-float|least-positive-short-float|least-positive-single-float|long-float-epsilon|long-float-negative-epsilon|most-negative-double-float|most-negative-fixnum|most-negative-long-float|most-negative-short-float|most-negative-single-float|most-positive-double-float|most-positive-fixnum|most-positive-long-float|most-positive-short-float|most-positive-single-float|multiple-values-limit|pi|short-float-epsilon|short-float-negative-epsilon|single-float-epsilon|single-float-negative-epsilon)[)\\s]"))

;; TODO implement window abstraction with borders
;; TODO smarter redisplay computation - XXX buffers are assumed not to change rn
;; TODO more portable terminal code

(defun %tui-redisplay (tui &key force-p)
  (let ((start-time (get-internal-real-time)))
    ;; XXX this is really lazy
    (format t "~C[48;2;0;43;54;38;2;131;148;150m" (code-char 27)) ;#839496
    (dolist (window (windows tui))
      (unless (zerop (length (window-buffer window)))
        (ti:tputs ti:change-scroll-region
                  (1- (window-y window))
                  (- (window-height window) 2))
        (let ((last-top (last-top-line window))
              (top (top-line window))
              start-line end-line
              visual-line)
          (cond ((or force-p (>= (abs (- last-top top)) (window-height window)))
                 (ti:tputs ti:clear-screen)
                 (setf start-line top
                       end-line (+ top (- (window-height window) 2))
                       visual-line 1))
                ((= top last-top)
                 (return))
                ((> top last-top)
                 (ti:tputs ti:parm-index (- top last-top))
                 (setf start-line (+ last-top (1- (window-height window)))
                       end-line (+ top (- (window-height window) 2))
                       visual-line (+ (1- (window-y window))
                                      (- (window-height window) (- top last-top)))))
                (t ; last-top > top
                 (ti:tputs ti:parm-rindex (- last-top top))
                 (setf start-line top
                       end-line (1- last-top)
                       visual-line 1)))
          (setf end-line (min end-line (line-count (window-buffer window))))
          ;; (unless force-p
          ;;   (format t "~C[48;2;100;20;40m" (code-char 27)))
          (loop :with buffer = (window-buffer window)
                :for line :from start-line :to end-line
                :for start-offset = (line-number-offset buffer start-line)
                  :then next-offset
                :for next-offset = (line-number-offset buffer (1+ line))
                :do (ti:tputs ti:cursor-address (1- visual-line) 0)
                    (write-string
                     (with-output-to-string (displayed-string)
                       (loop :with macros :and special-ops :and globals
                             :initially (ppcre-custom:do-scans
                                            (s e reg-starts reg-ends
                                             *cl-macro-regex*
                                             buffer (setf macros (nreverse macros))
                                             :start start-offset
                                             :end
                                             (min next-offset ;WITH-STANDARD-IO-SYNTAX
                                                  (+ start-offset (window-width window) 22))
                                             :accessor #'char)
                                          (push (cons (aref reg-starts 0) (aref reg-ends 0))
                                                macros))
                                        (ppcre-custom:do-scans
                                            (s e reg-starts reg-ends
                                             *cl-special-operator-regex*
                                             buffer (setf special-ops (nreverse special-ops))
                                             :start start-offset
                                             :end
                                             (min next-offset ;MULTIPLE-VALUE-PROG1
                                                  (+ start-offset (window-width window) 19))
                                             :accessor #'char)
                                          (push (cons (aref reg-starts 0) (aref reg-ends 0))
                                                special-ops))
                                        (ppcre-custom:do-scans
                                            (s e reg-starts reg-ends
                                             *cl-global-regex*
                                             buffer (setf globals (nreverse globals))
                                             :start start-offset
                                             :end
                                             (min next-offset ;MULTIPLE-VALUE-PROG1
                                                  (+ start-offset (window-width window) 19))
                                             :accessor #'char)
                                          (push (cons (aref reg-starts 0) (aref reg-ends 0))
                                                globals))
                             :with width = 0
                             :for idx :from start-offset :below (1- next-offset)
                             :for (length displayed) = (multiple-value-list
                                                        (term:wide-character-width
                                                         (char buffer idx)))
                             :while (<= (incf width length) (window-width window))
                             :do (when (and globals (= idx (caar globals)))
                                   (format displayed-string "~C[38;2;203;75;22m"
                                           (code-char 27)))
                                 (when (and special-ops (= idx (caar special-ops)))
                                   (format displayed-string "~C[38;2;181;137;0m"
                                           (code-char 27)))
                                 (when (and macros (= idx (caar macros)))
                                   (format displayed-string "~C[38;2;133;153;0m"
                                           (code-char 27)))
                                 (write-string displayed displayed-string)
                                 (when (and macros (= idx (1- (cdar macros))))
                                   (format displayed-string "~C[38;2;131;148;150m"
                                           (code-char 27))
                                   (pop macros))
                                 (when (and special-ops (= idx (1- (cdar special-ops))))
                                   (format displayed-string "~C[38;2;131;148;150m"
                                           (code-char 27)))
                                 (when (and globals (= idx (1- (cdar globals))))
                                   (format displayed-string "~C[38;2;131;148;150m"
                                           (code-char 27))))))
                    (incf visual-line))
          (setf (last-top-line window) top)))
      (ti:tputs ti:cursor-address (1- (window-height window)) (1- (window-y window)))
      (format t "~C[48;2;7;54;66;38;2;101;123;131m" (code-char 27))
      ;; per window status
      (let ((status-line ; XXX assuming ascii
              (with-output-to-string (status-string)
                (format status-string " - ")
                (format status-string "~A | " (or (buffer-name (window-buffer window))
                                                  "an unnamed buffer"))
                (format status-string "line: ~d/~d | "
                        (top-line window) (line-count (window-buffer window)))
                (format status-string "redisplayed in ~5f secs"
                        (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))))
        (write-string
         (subseq status-line 0 (min (length status-line) (window-width window)))))
      (ti:tputs ti:clr-eol) ;assuming back-color-erase
      (finish-output))
    nil))

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

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer))
