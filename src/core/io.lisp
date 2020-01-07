(defpackage :vico-core.io
  (:use :cl)
  (:export #:*default-file-encoding*
           #:guess-encoding
           #:file-length-in-characters
           #:file-to-bytes
           #:text-file-to-string
           #:file-writable-p))
(in-package :vico-core.io)

(defun file-truename (pathname)
  (let ((truename (uiop:probe-file* pathname :truename t)))
    (cond ((not truename)
           (error "Could not find pathname ~S" pathname))
          ((uiop:directory-pathname-p truename)
           (error "Pathname ~S is a directory" truename))
          (t
           truename))))

(defvar *default-file-encoding* :utf-8)

(defun guess-encoding (pathname)
  "Given a valid file pathname, return its (guessed) encoding."
  (let* ((%encoding (asdf-encodings:detect-file-encoding pathname)))
    (cond ((eq %encoding :default)
           :utf-8)
          ((eq %encoding :utf-7) ; compatible with ascii
           :ascii)
          ((eq %encoding :iso-2022-kr)
           :latin1)
          (t
           %encoding))))

(defun file-length-in-characters (pathname)
  (with-open-file (stream (file-truename pathname))
    (file-length stream)))

(defun file-to-bytes (pathname)
  "Takes a resolved non-directory pathname and returns its contents as an octet vector."
  (let* ((truename (file-truename pathname))
         (file-length (osicat-posix:stat-size (osicat-posix:stat truename))))
    (with-open-file (stream truename :element-type '(unsigned-byte 8))
      (let ((buffer (make-array file-length :element-type '(unsigned-byte 8))))
        (read-sequence buffer stream)
        buffer))))

(defun text-file-to-string (pathname &optional (guess-encoding t))
  "Return the contents of PATHNAME as a string. When guess-encoding is nil, defaults to
*DEFAULT-ENCODING*."
  (let ((truename (file-truename pathname)))
    (let ((encoding (if guess-encoding
                        (guess-encoding truename)
                        *default-file-encoding*)))
      (babel:octets-to-string (file-to-bytes truename) :encoding encoding))))

;; adapted from the portable hemlock source code
(defun file-writable-p (pathname)
  "File-writable accepts a pathname and returns T if the current process can write it, and
NIL otherwise. Also if the file does not exist return T."
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t) ; => t
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io)) ; => t unless perms changed
                            (file-writable-p pathname))))) ; somebody created it
    (file-error (e)
      (declare (ignore e))
      nil)))
