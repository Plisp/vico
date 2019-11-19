;;;; frontend interface

(defpackage :vico-lib.ui
  (:use :cl)
  (:local-nicknames (:condition :vico-core.conditions))
  (:export
   :ui :*frontends*
   :windows
   ;; window protocol
   :window :make-window
   :window-buffer
   :window-x :window-y
   :window-width :window-height
   :window-string-width :window-line-height
   :move-window :resize-window
   :raise-window :lower-window
   :redisplay-window))
