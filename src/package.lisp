(in-package :cl-user)

;; this package exports user-facing routines

(defpackage :vico-lib
  (:use :cl :vico-core)
  (:shadowing-import-from :vico-core
   :length :char :subseq)
  #.`(:export
      ,@(loop :for sym being each external-symbol of (find-package :cl)
              :collect sym)
      ,@(loop :for sym being each external-symbol of (find-package :vico-core)
              :collect sym)))
