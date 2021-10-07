(defpackage :vico-core.ui
  (:use :cl))
(in-package :vico-core.ui)

;; thanks to JMC for this code which implements exactly the windowing format I came up with
;; I will rewrite it sometime

(defun calc-box (list limit)
  "Divides limit by amount of items in list."
  (let ((free 0)      ;views with unspecified sizes
        (first-free ) ;We add all division remainders to the first unsized
        (free-space limit)
        (result '()))
    (loop :for item :in list :do
      (typecase item
        (window (let ((res (cons item nil)))
                  (push res result)
                  (when (eql free 0) (setf first-free res))
                  (incf free)))
        ;; cons means division or param. if cdr is a symbol it's a param
        (cons   (if (listp (cdr item)) ; division?
                    (progn   ; when division, push the whole division
                      (let ((res (cons item nil)))
                        (push res result)
                        (when (eql free 0) (setf first-free res))
                        (incf free)))
                    (let* ((unit (car item))
                           (param (cdr item))
                           (measure (typecase param
                                      (integer (decf free-space param) param) ; pixels
                                      (real (let ((amount (round (* param limit))))
                                              (decf free-space amount) ; percentage
                                              amount))
                                      (cons nil)))) ; means we've got a division
                      (push (cons unit measure) result))))))
    (let ((remaining free-space))
      (multiple-value-bind (division remainder)
          (floor free-space free)
        (dolist (unit result)
          (when (null (cdr unit))
            (rplacd unit division)))
        (when (plusp remaining) (incf (cdr first-free) remainder))))
    (nreverse result)))

(defun calc-layout (layout size &optional (horizontal t) (x 0) (y 0))
  "Takes a layout and size (width . height) and returns a list of items with their dimensions and locations."
  (rotatef (car size) (cdr size))
  ;; XXX temporary hack. I should rewrite this
  (let* ((width (car size))
         (height (cdr size))
         (current (calc-box layout height))
         (cur-x x) (cur-y y)
         (result '()))
    (loop :for (box . size) :in current
          :do (let ((new-x (if horizontal (+ cur-x size) (+ cur-x width)))
                    (new-y (if horizontal (+ cur-y width) (+ cur-y size))))
                (if (consp box)
                    (progn
                      (setf result
                            (append (nreverse
                                     (calc-layout box
                                                  (if horizontal
                                                      `(,size . ,width)
                                                      `(,size . ,width))
                                                  (if horizontal
                                                      nil
                                                      t)
                                                  cur-x cur-y))
                                    result))
                      (if horizontal
                          (setf cur-x new-x)
                          (setf cur-y new-y)))
                    (progn
                      (push `(,box ,(if horizontal
                                        `(,size . ,width)
                                        `(,width . ,size))
                                   (,cur-x . ,cur-y))
                            result)
                      (if horizontal
                          (setf cur-x new-x)
                          (setf cur-y new-y))))))
    (when (find-if #'minusp (remove-if-not #'numberp (alexandria:flatten result)))
      (error "size insufficient for layout"))
    (nreverse result)))
