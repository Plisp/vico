;;; current official (and only) frontend

(asdf:defsystem :vico-term
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A pure lisp terminal frontend for vico."
  :license "BSD 3-clause license"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:vico-lib :cffi :terminfo :uncursed)
  :pathname "term"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "util")
               (:file "impl")
               (:file "main")))
