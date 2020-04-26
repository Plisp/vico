(defpackage vico-core.key-event
  (:use :cl :alexandria)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:ev :vico-core.evloop)
                    (:ui :vico-core.ui))
  (:export #:key-event #:make-key-event
           #:key-name #:key-window
           #:*default-keybinds*))
(in-package :vico-core.key-event)

(defstruct (key-event (:conc-name key-))
  (name :null)
  window)

(defvar *default-keybinds* nil)

(defmethod ev:handle-event ((event key-event))
  (let* ((window (key-window event))
         (val (when-let ((binding (assoc-value ;; (buf:keybinds (ui:window-buffer window))
                                   *default-keybinds*
                                   (key-name event))))
                (funcall binding window))))
    ;;(print (key-name event)) (force-output)
    (when (and (not val) (characterp (key-name event))
               (or (not (< (char-code (key-name event)) 32))
                   (char= (key-name event) #\newline))
               (not (<= 127 (char-code (key-name event)) 160)))
      (buf:insert-at (ui:window-point window) (string (key-name event)))
      (assert (buf:cursor-valid-p (ui:window-point window)))
      (ui:move-point window 1))
    (setf ev:*editor-arg* 1)
    (if (eq val :force-redisplay)
        (ui:redisplay (ui:window-ui window) :force-p t)
        (ui:redisplay (ui:window-ui window))) ; feedback is important!
    val))
