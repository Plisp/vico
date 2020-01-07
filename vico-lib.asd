;;;; library

(defsystem :vico-lib
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "Editor utilities"
  :license "BSD 3-clause"
  :depends-on
  (:vico-core
   :async-process)
  :pathname "src"
  :components ((:file "package")))
