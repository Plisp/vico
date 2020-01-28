;; editor core

(defsystem :vico-core
  :depends-on
  (:alexandria
   :atomics
   :asdf-encodings ;TODO replace
   :babel
   :bordeaux-threads
   :cl-ppcre-custom
   :cl-unicode
   :lparallel
   :osicat
   :safe-queue
   :trivial-features
   :trivial-garbage
   ;;:trivial-timers
   ;;:uax-14
   )
  :pathname "src/core"
  :components ((:file "conditions")
               (:file "io")
               (:file "buffer" :depends-on ("conditions"))
               (:file "piece-table-backend" :depends-on ("buffer"))
               (:file "undoable-buffer" :depends-on ("buffer"))
               (:file "marked-buffer" :depends-on ("buffer"))
               (:file "concurrency-util")
               (:file "ui-base")
               (:file "ui-window" :depends-on ("ui-base"))
               (:file "event" :depends-on ("concurrency-util" "ui-base" "ui-window"))
               (:file "key" :depends-on ("event" "ui-base"))
               (:file "standard-buffer" :depends-on ("concurrency-util" "key"))
               (:file "graphemes")
               (:file "default-keybinds")
               (:file "package" :depends-on ("conditions"
                                             "io"
                                             "buffer"
                                             "piece-table-backend"
                                             "undoable-buffer"
                                             "marked-buffer"
                                             "concurrency-util"
                                             "ui-base"
                                             "ui-window"
                                             "event"
                                             "key"
                                             "standard-buffer"
                                             "graphemes"
                                             "default-keybinds"))))
