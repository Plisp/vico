;;; current official (and only) frontend

(asdf:defsystem :vico-term
  :build-pathname "vico"
  :entry-point "vico-term:main"
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A pure lisp terminal frontend for vico."
  :license "BSD 3-clause license"
  :depends-on (:vico-lib :uncursed :trivial-clipboard)
  :pathname "term"
  :serial t
  :components ((:file "package")
               (:file "bindings")
               (:file "impl")
               (:file "main")))
