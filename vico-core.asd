;;;; editor core library

(asdf:defsystem :vico-core
  :depends-on
  (:alexandria       ;reliable
   :atomics          ;reliable
   :bordeaux-threads ;reliable
   :cffi
   ;;:cl-unicode       ;adapt/contribute
   :trivial-features ;reliable
   :trivial-file-size;reliable
   :trivial-garbage  ;reliable
   ;;:uax-14
   ;; *not in quicklisp*
   :polymorph.maths
   :polymorph.data-structures
   )
  :pathname "src/core"
  :components ((:file "buffer")
               ;; probably unportable, load separately if needed. needs PCRE
               (:file "libst")
               (:file "editor")))
