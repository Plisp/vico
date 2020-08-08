;;; current official (and only) frontend

(asdf:defsystem :vico-term
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A pure lisp terminal frontend for vico."
  :license "BSD 3-clause license"
  :depends-on (:vico-lib :uncursed)
  :pathname "term"
  :serial t
  :components ((:file "package")
               (:file "impl")
               (:file "main")))
