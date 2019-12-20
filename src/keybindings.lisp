(defpackage vico-lib.key-event
  (:use :cl :alexandria)
  (:local-nicknames (:ev :vico-lib.evloop)
                    (:ui :vico-lib.ui))
  (:export #:key-event #:make-key-event
           #:key-name #:key-window
           #:*default-keybinds*))
(in-package :vico-lib.key-event)

(defvar *default-keybinds*
  (list (cons :c-l (lambda (window)
                     (ui:redisplay-window window)))

        (cons :c-c (lambda (window)
                     (ui:quit (ui:window-ui window))
                     (ev:quit-editor-loop ev:*editor*)))

        (cons :c-e (lambda (window)
                     (incf (ui:top-line window))
                     (ui:redisplay-window window)))

        (cons :c-y (lambda (window)
                     (when (> (ui:top-line window) 1)
                       (decf (ui:top-line window)))
                     (ui:redisplay-window window)))))
