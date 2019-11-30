;;;; file io utilities

(defpackage :vico-core.io
  (:use :cl)
  (:export #:*default-file-encoding*
           #:text-file-to-string
           #:file-writable-p))
(in-package :vico-core.io)

(defun file-to-bytes (pathname)
  "Takes a valid pathname and returns its contents as a octet vector."
  (let ((file-length (trivial-file-size:file-size-in-octets pathname)))
    (with-open-file (stream pathname :element-type '(unsigned-byte 8))
      (let ((buffer (make-array file-length :element-type '(unsigned-byte 8))))
        (read-sequence buffer stream)
        buffer))))

(defvar *default-file-encoding* :utf-8)

;; from the portable hemlock source code
(defun file-writable-p (pathname)
  "File-writable accepts a pathname and returns T if the current process can write it, and
NIL otherwise. Also if the file does not exist return T."
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t)
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io))
                            t))))
    (file-error (err)
      (declare (ignore err))
      nil)))

(defun guess-encoding (pathname)
  "Given a valid pathname, return the (guessed) encoding of the corresponding file."
  (let* ((%encoding (asdf-encodings:detect-file-encoding pathname)))
    (cond ((eq %encoding :default)
           :utf-8)
          ((eq %encoding :utf-7) ; compatible with ascii
           :ascii)
          ((eq %encoding :iso-2022-kr)
           :latin1)
          (t
           %encoding))))

;; FIXME handle BOM

(defun text-file-to-string (pathname &optional (guess-encoding t))
  "Return the contents of PATHNAME as a string. When guess-encoding is nil, defaults to
*DEFAULT-ENCODING*."
  (let ((truename (uiop:probe-file* pathname)))
    (cond ((not truename)
           (error "Could not find pathname ~S" pathname))
          ((uiop:directory-pathname-p truename)
           (error "Pathname ~S is a directory" truename))
          (t
           (let ((encoding (if guess-encoding
                               (guess-encoding truename)
                               *default-file-encoding*)))
             (babel:octets-to-string (file-to-bytes truename) :encoding encoding))))))
