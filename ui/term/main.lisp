;;;;
;;;
;;
;; TUI entry point and further definitions
;;
;;;
;;;;

(in-package :vico-term)

;;; sigwinch handling

(defconstant +sigwinch+ 28)

(cffi:defcfun ("signal" c-signal) :pointer
  (signo :int)
  (handler :pointer))

(defun handle-winch ()
  (dolist (ui (frontends *editor*))
    (when (typep ui 'tui)
      (let* ((dimensions (term:get-terminal-dimensions))
             (xscale (/ (cdr dimensions) (ui:frame-width ui)))
             (yscale (/ (car dimensions) (ui:frame-height ui))))
        (mapcar
         (lambda (window)
           (setf (ui:window-width window)  (truncate (* (ui:window-width window)  xscale))
                 (ui:window-height window) (truncate (* (ui:window-height window) yscale))))
         (ui:windows ui))
        (setf (ui:frame-width ui)  (cdr dimensions)
              (ui:frame-height ui) (car dimensions))
        (bt:interrupt-thread (ui:ui-thread ui) (lambda () (throw 'redisplay nil)))
        (return)))))

(cffi:defcallback sigwinch-handler :void ((signo :int))
  (declare (ignore signo))
  (handle-winch))

;;; main

(defvar +c-l+ (make-instance 'key-event))
(defvar +c-c+ (make-instance 'key-event))
(defvar +c-e+ (make-instance 'key-event))
(defvar +c-y+ (make-instance 'key-event))

(defun start-tui-loop (tui)
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl)
        (*terminal-io* *standard-output*))

    (push tui (frontends *editor*))

    (let (orig-termios init-done original-handler)
      (unwind-protect
           (progn
             (princ "C-l redraws, C-c quits. glhf ;)")
             (finish-output)

             (setf orig-termios (term:setup-terminal-input))
             (ti:set-terminal (uiop:getenv "TERM"))
             (ti:tputs ti:clear-screen) ;TODO line wrap
             (mapcar (lambda (window) (ui:redisplay-window window)) (ui:windows tui))

             (catch 'quit-ui-loop
               (loop
                  (if-let ((window
                            (catch 'redisplay ; KLUDGE
                              (unless init-done
                                (setf original-handler
                                      (c-signal +sigwinch+ (cffi:callback sigwinch-handler)))
                                (setf init-done nil))
                              (loop
                                 (queue-event (event-queue *editor*)
                                              (let ((ev (term:read-terminal-event)))
                                                (when (characterp ev)
                                                  (cond ((char= ev #\Page) +c-l+)
                                                        ((char= ev #\Etx) +c-c+)
                                                        ((char= ev #\Enq) +c-e+)
                                                        ((char= ev #\Em) +c-y+)))))))))
                    (ui:redisplay-window window)
                    (mapcar (lambda (window) (ui:redisplay-window window)) (ui:windows tui)))))

             (deletef (frontends *editor*) tui))
        (when orig-termios
          (term:restore-terminal-input orig-termios))
        (when original-handler
          (c-signal +sigwinch+ original-handler))))))

(defun main (filename)
  (let* ((*editor* (make-instance 'editor))
         (terminal-dimensions (term:get-terminal-dimensions))
         (tui (make-instance 'tui :width  (cdr terminal-dimensions)
                                  :height (car terminal-dimensions)))
         (initial-buffer
           (make-instance 'vico-lib.standard-buffer:standard-buffer
                          :initial-contents (text-file-to-string filename)
                          :inherit-keybinds nil
                          :local-keybinds (pairlis (list +c-l+ +c-c+ +c-e+ +c-y+)
                                                   (list (lambda (context)
                                                           (declare (ignore context))
                                                           (bt:interrupt-thread
                                                            (ui:ui-thread tui)
                                                            (lambda ()
                                                              (throw 'redisplay
                                                                (ui:focused-window tui)))))
                                                         (lambda (context)
                                                           (declare (ignore context))
                                                           (bt:interrupt-thread
                                                            (ui:ui-thread tui)
                                                            (lambda ()
                                                              (throw 'quit-ui-loop nil)))
                                                           (throw 'quit-event-loop nil))
                                                         (lambda (context)
                                                           (declare (ignore context))
                                                           (bt:interrupt-thread
                                                            (ui:ui-thread tui)
                                                            (lambda ()
                                                              (incf (vico-term.impl::%top-line
                                                                     (ui:focused-window tui)))
                                                              (throw 'redisplay
                                                                (ui:focused-window tui)))))
                                                         (lambda (context)
                                                           (declare (ignore context))
                                                           (bt:interrupt-thread
                                                            (ui:ui-thread tui)
                                                            (lambda ()
                                                              (when (> (vico-term.impl::%top-line
                                                                        (ui:focused-window tui)) 1)
                                                                (decf (vico-term.impl::%top-line
                                                                       (ui:focused-window tui))))
                                                              (throw 'redisplay
                                                                (ui:focused-window tui)))))))))
         (initial-window (ui:make-window tui 1 1
                                         (cdr terminal-dimensions) (car terminal-dimensions)
                                         :buffer initial-buffer)))
    (setf (ui:focused-window tui) initial-window)
    (push initial-window (ui:windows tui))
    (push initial-buffer (buffers *editor*))
    (setf (slot-value +c-l+ 'key-window) initial-window
          (slot-value +c-c+ 'key-window) initial-window
          (slot-value +c-e+ 'key-window) initial-window
          (slot-value +c-y+ 'key-window) initial-window)

    (setf (ui:ui-thread tui) (bt:make-thread (lambda () (start-tui-loop tui))
                                             :name "tui thread"
                                             :initial-bindings `((*editor* . ,*editor*))))
    (start-event-loop *editor*)))
