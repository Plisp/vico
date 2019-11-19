(in-package :vico-lib)

(defmacro without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts
    #+ccl ccl:without-interrupts
    #+ecl ext:without-interrupts
    #-(or sbcl ccl ecl) progn
    ,@body))

(defmacro with-local-interrupts (&body body)
  `(#+sbcl sb-sys:with-local-interrupts
    #+ccl ccl:with-interrupts-enabled
    #+ecl ext:with-local-interrupts
    #-(or sbcl ccl ecl) progn
    ,@body))

;; (defun set-thread-pool-size (&optional n)
;;   (eager-future2:advise-thread-pool-size
;;    (or n (+ (cl-cpus:get-number-of-processors) 3))))
