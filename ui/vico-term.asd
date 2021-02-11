;;; current official (and only) frontend

(asdf:defsystem :vico-term
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "vico"
  :entry-point "vico-term:main"
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A pure lisp terminal frontend for vico."
  :license "BSD 3-clause license"
  :depends-on (:vico-lib :uncursed)
  :pathname "term"
  :serial t
  :components ((:file "package")
               (:file "bindings")
               (:file "impl")
               (:file "main")))

(in-package #:org.shirakumo.deploy)
(defmethod asdf:perform ((o deploy-op) (c (eql (asdf:find-system :vico-term))))
  (status 0 "Running load hooks.")
  (run-hooks :load :system c :op o)
  (status 0 "Gathering system information.")
  (destructuring-bind (file data) (asdf:output-files o c)
    (setf *foreign-libraries-to-reload* (remove-if-not #'library-open-p
                                                       (remove-if #'library-dont-open-p (list-libraries))))
    (status 1 "Will load the following foreign libs on boot:
      ~s" *foreign-libraries-to-reload*)
    (status 0 "Deploying files to ~a" data)
    (ensure-directories-exist file)
    (ensure-directories-exist data)
    (setf *data-location* (find-relative-path-to data (uiop:pathname-directory-pathname file)))
    (run-hooks :deploy :directory data :system c :op o)
    (status 0 "Running build hooks.")
    (run-hooks :build :system c :op o)
    (status 0 "Dumping image to ~a" file)
    (setf uiop:*image-dumped-p* :executable)
    (setf (fdefinition 'deployed-p) (lambda () T))
    #+windows
    (setf file (make-pathname :type "exe" :defaults file))
    #+(and windows ccl)
    (ccl:save-application file
                          :prepend-kernel T
                          :purify T
                          :toplevel-function #'uiop:restore-image
                          :application-type
                          (if (uiop:featurep :deploy-console)
                              :console
                              :gui))
    #-(and windows ccl)
    (apply #'uiop:dump-image file
           (append '(:executable T)
                   #+(and sbcl os-windows)
                   `(:application-type
                     ,(if (uiop:featurep :deploy-console)
                          :console
                          :gui))))))

