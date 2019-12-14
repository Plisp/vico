;;;;
;;;
;;; implementation of terminal frontend interface
;;;
;;;;

(in-package :vico-term.impl)

(defclass tui (ui)
  ((focused-window :initarg :focused-window
                   :accessor focused-window
                   :type window)
   (windows :initform (list)
            :accessor windows
            :type list)))

;; TODO must move %top-line using a mark - may end up anywhere in a line after
;; deletion - thus search backwards for linefeed TODO buffer search interface
;; TODO initargs should be obvious from context?
(defclass tui-window (window)
  ((%top-line :initform 1
              :accessor %top-line)
   (x :initarg :x
      :accessor window-x)
   (y :initarg :y
      :accessor window-y)
   (width :initarg :width
          :accessor window-width)
   (height :initarg :height
           :accessor window-height)
   (buffer :initarg :buffer
           :accessor window-buffer))
  (:documentation "The only type of window"))

(defmethod make-window ((ui tui) x y width height &key buffer floating)
  (declare (ignorable floating))
  (make-instance 'tui-window :ui ui
                             :x x :y y :width width :height height
                             :buffer buffer))

;; TODO finish implementing window protocol
;; TODO implement window abstraction with borders

;; TODO smarter redisplay computation using edit history
(defmethod redisplay-window ((window tui-window) &key force-p)
  (declare (ignore force-p))
  (loop :initially (ti:tputs ti:clear-screen)
                   (ti:tputs ti:cursor-address 0 0)
                   (finish-output)
        :for line from (%top-line window) to (window-height window)
        :while (< line (line-count (window-buffer window)))
        :for line-offset = (line-number-offset (window-buffer window) line)
          then next-line-offset
        :for next-line-offset = (line-number-offset (window-buffer window) (1+ line))
        :for text = (subseq (window-buffer window) line-offset next-line-offset)
        :do (write-string text)
            (ti:tputs ti:cursor-address line 0)))
