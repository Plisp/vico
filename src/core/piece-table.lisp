;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; piece table rewrite
;;
;; TODO mmap()
;; TODO babel
;; TODO implement interface
;;

(defpackage :vico-core.piece-table
  (:use :cl :alexandria)
  (:import-from :vico-core.buffer :*max-buffer-size*)
  (:local-nicknames (:buf :vico-core.buffer) (:enc :babel) (:ffi :cffi))
  (:export))
(in-package :vico-core.piece-table)

(defun required-arg (a)
  (error "struct arg ~a is required" a))

(deftype idx () `(integer 0 #.*max-buffer-size*))

(defstruct data-buffer
  "manages a pointer to a foreign vector of octets"
  (len 0 :type idx)
  (capacity 0 :type idx)
  (data (ffi:null-pointer) :type ffi:foreign-pointer)
  (lfs-len 0 :type idx)
  (lfs (make-array 0 :element-type 'idx :fill-pointer t) :type (array idx))
  (type :alloc :type (member :mmap :alloc)))

(defstruct node
  "SIZE is in octets"
  next prev
  (data (ffi:null-pointer) :type ffi:foreign-pointer)
  (size 0 :type idx)
  (lf-offset 0 :type idx))

(defstruct (piece-table (:conc-name pt-))
  (size 0 :type idx)
  (line-count 0 :type idx)
  (data-buffers (list) :type list)
  (end-cache nil)
  (sentinel1)
  (sentinel2))
