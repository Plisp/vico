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
   :trivial-timers
   :uax-14)
  :pathname "src"
  :components ((:file "concurrency-util")
               (:file "ui-base")
               (:file "ui-window" :depends-on ("ui-base"))
               (:file "event" :depends-on ("concurrency-util" "ui-base" "ui-window"))
               (:file "keybindings" :depends-on ("event" "ui-base"))
               (:file "key" :depends-on ("event" "ui-base" "standard-buffer"))
               (:file "standard-buffer" :depends-on ("concurrency-util" "keybindings"))
               (:file "package" :depends-on ("concurrency-util"
                                             "ui-base"
                                             "ui-window"
                                             "event"
                                             "key"
                                             "standard-buffer"))))
