;;;; entry point and further definitions

(in-package :vico-term)

(defvar +r+ (make-instance 'key-event))
(defvar +c-c+ (make-instance 'key-event))

(defun main (filename)
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl)
        (*terminal-io* *standard-output*))
    (let* ((editor (make-instance 'editor))
           (tui (make-instance 'tui :ui-thread (concurrency:current-thread)))
           (initial-buffer
             (make-instance 'vico-lib.standard-buffer:standard-buffer
                            :initial-contents (text-file-to-string filename)
                            :inherit-keybinds nil
                            :local-keybinds (pairlis (list +r+ +c-c+)
                                                     (list (lambda (context)
                                                             (declare (ignore context))
                                                             (bt:interrupt-thread
                                                              (ui:ui-thread tui)
                                                              (lambda ()
                                                                (throw 'redisplay nil))))
                                                           (lambda (context)
                                                             (declare (ignore context))
                                                             (bt:interrupt-thread
                                                              (ui:ui-thread tui)
                                                              (lambda ()
                                                                (throw 'quit-ui-loop nil)))
                                                             (throw 'quit-event-loop nil))))))
           (dimensions (term:get-terminal-dimensions))
           (initial-window (ui:make-window tui 1 1 (cdr dimensions) (car dimensions)
                                           :buffer initial-buffer)))

      (setf *editor* editor)
      (setf (slot-value *editor* 'event-loop-thread)
            (bt:make-thread (lambda ()
                              (start-event-loop *editor*))
                            :name "editor event-loop thread"))

      (push tui (frontends editor))
      (push initial-window (ui:windows tui))
      (push initial-buffer (buffers *editor*))
      (setf (slot-value +r+ 'key-window) initial-window)
      (setf (slot-value +c-c+ 'key-window) initial-window)

      (let (orig-termios)
        (unwind-protect
             (progn
               (setf orig-termios (term:setup-terminal-input))
               (ti:set-terminal (uiop:getenv "TERM"))
               (ti:tputs ti:clear-screen) ;TODO line wrap

               (princ "r draws, C-c quits. glhf ;)")
               (finish-output)

               (catch 'quit-ui-loop
                 (loop
                   (catch 'redisplay ; KLUDGE
                     (loop
                       (queue-event (event-queue *editor*)
                                    (let ((ev (term:read-terminal-event)))
                                      (when (characterp ev)
                                        (cond ((char= ev #\r) +r+)
                                              ((char= ev #\Etx) +c-c+)))))))
                   (ui:redisplay-window initial-window)))

               (deletef (frontends editor) tui))
          (when orig-termios
            (term:restore-terminal-input orig-termios)))))))
