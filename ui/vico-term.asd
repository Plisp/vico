;;; current official (and only) frontend

(defsystem :vico-term
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A pure lisp terminal frontend for vico."
  :license "tbd"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on
  (:vico-lib :cffi :terminfo)
  :pathname "term"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "util")
               (:file "impl")
               (:file "main")))
