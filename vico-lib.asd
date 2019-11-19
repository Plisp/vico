;;;; library

(defsystem :vico-lib
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A tickit frontend for vico."
  :license "tbd"
  :depends-on
  (:vico-core
   :async-process
   :bordeaux-threads
   :cl-cpus
   :cl-ppcre-custom
   :cl-unicode
   :safe-queue
   :trivial-features
   :trivial-timers)
  :pathname "src"
  :components ((:file "package")
               (:file "graphemes")
               (:module ui
                :serial t
                :components ((:file "package")
                             (:file "base")
                             (:file "window")))
               (:file "concurrency-util")
               (:file "event-loop")))
