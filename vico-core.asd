;; editor core

(defsystem :vico-core
  :depends-on
  (:asdf-encodings ;TODO replace
   :babel
   :cl-unicode
   :osicat
   :trivial-garbage)
  :pathname "src/core"
  :components ((:file "conditions")
               (:file "io")
               (:file "graphemes")
               (:file "buffer" :depends-on ("conditions"))
               (:file "piece-table-backend" :depends-on ("buffer"))
               (:file "undoable-buffer" :depends-on ("buffer"))
               (:file "marked-buffer" :depends-on ("buffer"))
               (:file "package" :depends-on ("conditions"
                                             "io"
                                             "graphemes"
                                             "buffer"
                                             "piece-table-backend"
                                             "undoable-buffer"
                                             "marked-buffer"))))
