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
   :lparallel
   :safe-queue
   :trivial-features
   :trivial-timers)
  :pathname "src"
  :components ((:file "concurrency-util")
               (:file "ui-base")
               (:file "ui-window" :depends-on ("ui-base"))
               (:file "event-loop" :depends-on ("concurrency-util" "ui-base" "ui-window"))
               (:file "package" :depends-on ("concurrency-util"
                                             "ui-base"
                                             "ui-window"
                                             "event-loop"))))
