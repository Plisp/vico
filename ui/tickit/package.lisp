;;;TODO

(defpackage :vico-tickit.util
  (:use :cl)
  (:export
   :wide-character-width))

(defpackage :vico-tickit
  (:use :cl)
  (:local-nicknames (:core :vico-core))
  (:export
   :main))
