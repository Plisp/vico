;;;; editor core library

(defsystem :vico-core
  :depends-on
  (:alexandria       ;reliable
   :atomics          ;reliable
   :asdf-encodings   ;TODO replace
   :babel            ;reliable
   :bordeaux-threads ;reliable
   :cl-ppcre-custom  ;reliable
   :cl-unicode       ;adapt/contribute
   :osicat           ;adapt/contribute
   :safe-queue       ;complete
   :trivial-features ;reliable
   :trivial-garbage  ;reliable
   ;;:uax-14
   )
  :pathname "src/core"
  :components ((:file "conditions")
               (:file "io")
               (:file "buffer" :depends-on ("conditions"))
               (:file "piece-table-backend" :depends-on ("buffer"))
               (:file "undoable-buffer" :depends-on ("buffer"))
               (:file "cursor-buffer" :depends-on ("buffer"))
               (:file "concurrency-util")
               (:file "ui-base")
               (:file "ui-window" :depends-on ("ui-base"))
               (:file "event" :depends-on ("concurrency-util" "ui-base" "ui-window"))
               (:file "key" :depends-on ("event" "ui-base"))
               (:file "syntax-highlighting")
               (:file "standard-buffer" :depends-on ("concurrency-util"
                                                     "key"
                                                     "syntax-highlighting"))
               (:file "graphemes")
               (:file "default-keybinds")
               (:file "package" :depends-on ("conditions"
                                             "io"
                                             "buffer"
                                             "piece-table-backend"
                                             "undoable-buffer"
                                             "cursor-buffer"
                                             "concurrency-util"
                                             "ui-base"
                                             "ui-window"
                                             "event"
                                             "key"
                                             "standard-buffer"
                                             "graphemes"
                                             "default-keybinds"))))
