;;; FIXME abandoned temporarily

(defsystem :vico-tickit
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A tickit frontend for vico."
  :license "tbd"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on
  (:vico-lib :cffi :cl-tickit)
  :pathname "tickit"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "util")
               (:file "interface")
               (:file "main")))
