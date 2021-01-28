(defpackage :vico-lib.logging
  (:use :cl)
  (:shadow :log)
  (:local-nicknames (:ed :vico-core.command-loop))
  (:export :log))
(in-package :vico-lib.logging)

(defun %log (string)
  (format t "~&[log]:~a~&" string)
  (finish-output))

(declaim (notinline log-command))
(defun log (message)
  (when (member :slynk *features*)
    (ed:queue-command (ed:command-queue ed:*editor*)
                      (list #'%log (princ-to-string message))))
  message)
