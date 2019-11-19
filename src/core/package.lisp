(in-package :cl-user)

;; this package collects (imports) external symbols from every package under this
;; directory and exports them

(eval-when (:compile-toplevel)
  (defvar *core-packages* '(:vico-core.conditions
                            :vico-core.io
                            :vico-core.buffer
                            :vico-core.buffer.piece-table
                            :vico-core.buffer.marked-buffer
                            :vico-core.buffer.undoable-buffer)))

(defpackage :vico-core
  #.`(:use ,@*core-packages*)
  #.`(:export
      ,@(loop :for package in *core-packages*
              :appending (loop :for sym being each external-symbol of package
                               :collect sym))))
