(defpackage :vico-lib.keyword-highlighting
  (:use :cl :alexandria)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:conditions :vico-core.conditions)
                    (:hl :vico-core.highlight)
                    (:ui :vico-core.ui))
  (:export #:keyword-highlighting-buffer
           #:symbol-at-point))
(in-package :vico-lib.keyword-highlighting)

(defclass keyword-highlighting-buffer (buf:buffer)
  ((symbol-at-point :initform ""
                    :type string
                    :accessor symbol-at-point)))

(defparameter *max-word-lookaround* 50)
(defun word-at-point (point)
  (let ((copy (buf:copy-cursor point)))
    (handler-case
        (unless (char= (buf:char-at copy) #\newline)
          (buf:move-cursor-chars* copy 1))
      (conditions:vico-bad-index ()))
    (buf:cursor-search-prev copy "\\n|\\w+" *max-word-lookaround*)
    (let ((length (buf:cursor-search-next copy "\\w+" *max-word-lookaround*)))
      (when (and length
                 (buf:cursor>= point copy)
                 (buf:cursor<= point (buf:cursor-next-char copy length)))
        (buf:cursor-prev-char copy length)
        (buf:subseq-at copy length)))))

(defmethod ui:buffer-styles-for-window append ((buffer keyword-highlighting-buffer)
                                              start end window)
  (let (spans)
    (flet ((add-span (start-cursor end-cursor)
             (push (make-instance 'ui:style-span
                                  :start start-cursor
                                  :end end-cursor
                                  :style (hl:make-style :bg #x073642 :fg #x2aa198))
                   spans)))
      (when-let (word (word-at-point (ui:window-point window)))
        (setf word (concatenate 'string "\\b" word "\\b")
              (symbol-at-point buffer) word)
        (loop :with it = (buf:copy-cursor start)
              :with left = (buf:cursor- end start)
              :for old = (buf:index-at it)
              :do (if-let (length (buf:cursor-search-next it word left))
                    (progn
                      (add-span (buf:copy-cursor it)
                                (buf:copy-cursor (buf:cursor-next-char it length)))
                      (decf left (- (buf:index-at it) old)))
                    (return spans)))))))
