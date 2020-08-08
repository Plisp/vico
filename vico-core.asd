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
   :mmap             ;reliable
   :safe-queue       ;reliable
   :static-vectors   ;reliable
   :trivial-features ;reliable
   :trivial-file-size ;reliable
   :trivial-garbage  ;XXX finalizers
   ;;:uax-14
   )
  :pathname "src/core"
  :components ((:file "conditions")
               (:file "io")
               (:file "concurrency")
               (:file "buffer" :depends-on ("conditions"))
               ;;(:file "piece-tree" :depends-on ("buffer"))
               (:file "piece-table" :depends-on ("buffer" "concurrency" "io"))
               ;;(:file "undoable-buffer" :depends-on ("buffer"))
               (:file "ui-base")
               (:file "ui-window" :depends-on ("ui-base"))
               (:file "event" :depends-on ("concurrency" "ui-base" "ui-window"))
               (:file "key" :depends-on ("event" "ui-base"))
               (:file "syntax-highlighting")
               ;; (:file "standard-buffer" :depends-on ("concurrency"
               ;;                                       "key"
               ;;                                       "syntax-highlighting"))
               (:file "graphemes")
               (:file "default-keybinds")
               (:file "package" :depends-on ("conditions"
                                             "io"
                                             "buffer"
                                             "piece-table"
                                             ;;"piece-tree"
                                             ;;"undoable-buffer"
                                             "concurrency"
                                             "ui-base"
                                             "ui-window"
                                             "event"
                                             "key"
                                             ;;"standard-buffer"
                                             "graphemes"
                                             "default-keybinds"))))
