(asdf:defsystem :vico-sdl
  :build-pathname "vico"
  :entry-point "vico-sdl:main"
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A SDL2 frontend for vico."
  :license "BSD 3-clause license"
  :depends-on (#:vico-lib
               #:cffi-libffi
               #:cl-environments
               #:font-discovery
               #:trivial-clipboard
               ;; *not on quicklisp*
               #:raw-bindings-sdl2 #:raw-bindings-sdl2-ttf)
  :pathname "sdl"
  :serial t
  :components ((:file "impl")))
