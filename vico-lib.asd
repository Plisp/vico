(asdf:defsystem :vico-lib
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "Editor utilities"
  :license "BSD 3-clause"
  :depends-on
  (:vico-core)
  :pathname "src"
  :components ((:file "package")
               (:file "logging")
               (:file "commands")
               (:file "keyword-highlighting")))
