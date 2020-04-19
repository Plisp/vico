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
  (name :null :type keyword)
  window)

(defvar *default-keybinds* nil)

(defmethod ev:handle-event ((event key-event))
  (let ((val (funcall (assoc-value (buf:keybinds (ui:window-buffer (key-window event)))
                                   (key-name event))
                      (key-window event))))
    (setf ev:*editor-arg* 1)
    ;; any event presumably could cause a screen update.
    ;; we usually let the frontend decide how to respond
    (dolist (ui (ev:frontends ev:*editor*))
      (if (eq val :force-redisplay)
          (ui:redisplay ui :force-p t)
          (ui:redisplay ui)))
    val))
