;;;; editor core library

(asdf:defsystem :vico-core
  :depends-on
  (:alexandria       ;reliable
   :atomics          ;reliable
   :asdf-encodings   ;TODO replace
   :babel            ;reliable
   :bordeaux-threads ;reliable
   :cl-ppcre         ;reliable, fork
   :cl-unicode       ;adapt/contribute
   :dynamic-mixins   ;reliable
   :mmap             ;reliable
   :safe-queue       ;reliable
   :static-vectors   ;reliable
   :trivial-features ;reliable
   :trivial-file-size;reliable
   :trivial-garbage  ;reliable
   ;;:uax-14
   )
  :pathname "src/core"
  :components ((:file "conditions")
               (:file "io")
               (:file "concurrency")
               (:file "graphemes")
               (:file "buffer" :depends-on ("conditions" "graphemes"))
               ;;(:file "piece-tree" :depends-on ("buffer"))
               (:file "piece-table" :depends-on ("buffer" "concurrency" "io"))
               ;; ui
               (:file "highlight")
               (:file "ui-base")
               (:file "ui-window" :depends-on ("ui-base" "buffer"))
               (:file "event" :depends-on ("concurrency" "ui-base" "ui-window"))
               (:file "key" :depends-on ("event" "ui-base"))
               (:file "default-keybinds")
               (:file "package" :depends-on ("conditions"
                                             "io"
                                             "buffer"
                                             "piece-table"
                                             ;;"piece-tree"
                                             "concurrency"
                                             "ui-base"
                                             "ui-window"
                                             "event"
                                             "key"
                                             "graphemes"
                                             "default-keybinds"))))
