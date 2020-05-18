(in-package :cl-user)

;; this package exports user-facing routines

(defpackage :vico-lib
  (:use :cl)
  (:local-nicknames (:core :vico-core))
  #.`(:export
      ,@(loop :for sym being each external-symbol of (find-package :cl)
              :collect sym)))
