(defpackage vico-lib.key-event
  (:use :cl :alexandria)
  (:local-nicknames (:ev :vico-lib.evloop)
                    (:ui :vico-lib.ui))
  (:export #:key-event #:make-key-event
           #:key-name #:key-window
           #:*default-keybinds*))
(in-package :vico-lib.key-event)

(defvar *default-keybinds*
  (list (list* :c-l (lambda (window)
                      (bt:interrupt-thread (ui:ui-thread (ui:window-ui window))
                                           (lambda ()
                                             (ui:redisplay-window window)))))
        (list* :c-c (lambda (window)
                      (ui:quit (ui:window-ui window))
                      (ev:quit-editor-loop ev:*editor*)))
        (list* :c-e (lambda (window)
                      (bt:interrupt-thread (ui:ui-thread (ui:window-ui window))
                                           (lambda ()
                                             (incf (ui:top-line window))
                                             (ui:redisplay-window window)))))
        (list* :c-y (lambda (window)
                      (bt:interrupt-thread (ui:ui-thread (ui:window-ui window))
                                           (lambda ()
                                             (when (> (ui:top-line window) 1)
                                               (decf (ui:top-line window)))
                                             (ui:redisplay-window window)))))))
