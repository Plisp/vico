(defpackage vico-lib.key-event
  (:use :cl :alexandria)
  (:local-nicknames (:core :vico-core)
                    (:ev :vico-lib.evloop)
                    (:ui :vico-lib.ui))
  (:export #:key-event #:make-key-event
           #:key-name #:key-window
           #:*default-keybinds*))
(in-package :vico-lib.key-event)

(defparameter *default-keybinds* ; TODO should be DEFVAR
  (list (cons :c-l (lambda (window)
                     (ui:redisplay-window window)))

        (cons :c-c (lambda (window)
                     (ui:quit (ui:window-ui window))
                     (ev:quit-editor-loop ev:*editor*)))

        (cons :c-e (lambda (window)
                     (when (< (1+ (ui:top-line window))
                              (core:line-count (ui:window-buffer window)))
                       (incf (ui:top-line window)))
                     (ui:redisplay-window window)))

        (cons :c-y (lambda (window)
                     (when (> (ui:top-line window) 1)
                       (decf (ui:top-line window)))
                     (ui:redisplay-window window)))))
