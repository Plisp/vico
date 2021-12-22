;;;; editor core library

(asdf:defsystem :vico-core
  :depends-on
  (:alexandria       ;reliable
   :bordeaux-threads ;reliable
   :cffi
   :safe-queue       ;reliable
   ;;:cl-unicode       ;adapt/contribute
   :trivial-features ;reliable
   :trivial-file-size;reliable
   :trivial-garbage  ;reliable
   )
  :pathname "src/core"
  :components ((:file "libst")
               (:file "buffer" :depends-on ("libst"))
               (:file "window")
               (:file "editor" :depends-on ("buffer" "window"))))
