(defpackage :vico-lib.logging
  (:use :cl)
  (:shadow :log)
  (:local-nicknames (:ed :vico-core.editor))
  (:export :log))
(in-package :vico-lib.logging)

(defun %log (&rest strings)
  (format t "~&[log]:~{~a~^ ~}~&" strings)
  (finish-output))

(declaim (notinline log-command))
(defun log (&rest messages)
  (when (member :slynk *features*)
    (ed:queue-command (ed:command-queue ed:*editor*)
                      (list* #'%log (mapcar #'princ-to-string messages))))
  (alexandria:lastcar messages))
