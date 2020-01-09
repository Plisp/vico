(defpackage vico-core.key-event
  (:use :cl :alexandria)
  (:local-nicknames (:buffer :vico-core.buffer)
                    (:ev :vico-core.evloop)
                    (:ui :vico-core.ui))
  (:export #:key-event #:make-key-event
           #:key-name #:key-window
           #:*default-keybinds*))
(in-package :vico-core.key-event)

(defstruct (key-event (:conc-name key-))
  (name :null :type keyword)
  window)

(defparameter *default-keybinds* nil) ; XXX should be DEFVAR

(defmethod ev:handle-event ((event key-event))
  (let ((val (funcall (assoc-value (buffer:keybinds (ui:window-buffer (key-window event)))
                                   (key-name event))
                      (key-window event))))
    ;; any event presumably could cause a screen update.
    ;; we usually let the frontend decide how to respond
    (dolist (ui (ev:frontends ev:*editor*))
      (if (eq val :force-redisplay)
          (ui:redisplay ui :force-p t)
          (ui:redisplay ui)))
    val))
