;;;; TODO

(defpackage :vico-term.util
  (:use :cl)
  (:export
   :wide-character-width
   :get-term-dimensions :set-term-dimensions
   :setup-terminal-input :restore-terminal-input))

(defpackage :vico-term
  (:use :vico-lib)
  (:local-nicknames (:ui :vico-lib.ui))
  (:export :main))
