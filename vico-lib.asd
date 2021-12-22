(asdf:defsystem :vico-lib
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "Editor utilities"
  :license "BSD 3-clause"
  :depends-on (:vico-core
               ;; *not in quicklisp*
               ;;:polymorph.maths
               ;;:polymorph.data-structures
               )
  :pathname "src"

  :components ((:file "package")
               ))
