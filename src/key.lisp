(in-package :vico-lib.key-event)

(defstruct (key-event (:conc-name key-))
  (name :null :type keyword)
  window)

(defmethod ev:handle-event ((event key-event))
  (funcall (assoc-value (vico-lib.standard-buffer:keybinds (ui:window-buffer (key-window event)))
                        (key-name event))
           (key-window event)))
