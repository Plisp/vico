;;;; library

(defsystem :vico-lib
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A tickit frontend for vico."
  :license "tbd"
  :depends-on
  (:vico-core
   :alexandria
   :async-process
   :bordeaux-threads
   :cl-cpus
   :cl-ppcre-custom
   :cl-unicode
   :lparallel
   :safe-queue
   :trivial-features
   :trivial-timers)
  :pathname "src"
  :components ((:file "package")
               (:file "graphemes")
               (:file "ui-base")
               (:file "ui-window")
               (:file "concurrency-util")
               (:file "event-loop")))
