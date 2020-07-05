(defpackage :vico-core.io
  (:local-nicknames (:enc :babel))
  (:use :cl)
  (:export #:*default-file-encoding*
           #:guess-encoding
           #:file-length-in-characters
           #:file-to-bytes
           #:text-file-to-string
           #:file-writable-p
           #:file-stream-fd))
(in-package :vico-core.io)

(defun file-stream-fd (stream)
  "Returns NIL if STREAM is not of type FILE-STREAM."
  (check-type stream file-stream)
  #+sbcl (sb-sys:fd-stream-fd stream)
  #+ccl (ccl:stream-device stream (ccl:stream-direction stream))
  #+ecl (si:file-stream-fd stream))

(defun file-truename (pathname)
  (let ((truename (uiop:probe-file* pathname :truename t)))
    (cond ((not truename)
           (error "Could not find pathname ~S" pathname))
          ((uiop:directory-pathname-p truename)
           (error "Pathname ~S is a directory, expected file" truename))
          (t
           truename))))

(defvar *default-file-encoding* :utf-8)

(defun guess-encoding (pathname)
  "Given a valid file pathname, return its (guessed) babel-compatible encoding."
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
         (file-length (trivial-file-size:file-size-in-octets truename)))
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
      (enc:octets-to-string (file-to-bytes truename) :encoding encoding))))

;; adapted from the portable hemlock source code
(defun file-writable-p (pathname)
  "File-writable accepts a pathname and returns T if the current process can write it,
and NIL otherwise. Also if the file does not exist return T."
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
                              (delete-file io)) ; => t unless failed (perhaps perms changed)
                            (file-writable-p pathname))))) ; somebody created it
    (file-error () nil)))
