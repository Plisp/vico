;; editor core

(defsystem :vico-core
  :depends-on
  (:asdf-encodings ;TODO replace
   :babel
   :trivial-file-size
   :trivial-garbage)
  :pathname "src/core"
  :serial t
  :components ((:file "conditions")
               (:file "io")
               (:file "buffer")
               (:module backend-buffers
                :serial t
                :components ((:file "piece-table")))
               (:file "undoable-buffer")
               (:file "marked-buffer")
               (:file "package")))
