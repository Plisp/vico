(defpackage :vico-lib.search
  (:use :cl :alexandria)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:conditions :vico-core.conditions)
                    (:hl :vico-core.highlight)
                    (:ui :vico-core.ui))
  (:export #:search-buffer
           #:symbol-at-point))
(in-package :vico-lib.search)

;;; symbol searching

(defun search-next-word (window arg)
  (with-accessors ((buffer ui:window-buffer)
                   (point ui:window-point))
      window
    (handler-case
        (dotimes (i arg)
          (buf:cursor-next-char point)
          (unless (buf:cursor-search-next point (symbol-at-point buffer))
            (buf:cursor-prev-char point)))
      (conditions:vico-bad-index ()))))

(defun search-prev-word (window arg)
  (with-accessors ((buffer ui:window-buffer)
                   (point ui:window-point))
      window
    (dotimes (i arg)
      (buf:cursor-search-prev point (symbol-at-point buffer)))))

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

;;; search field commands

(defclass search-field (buf:buffer)
  ((target :initform nil
           :type (or null ui:window)
           :accessor search-target
           :documentation "The window we're searching in")
   (match-length :initform nil
                 :accessor match-length)
   (search-buffer :initarg :buffer
                  :accessor search-buffer)
   (window :accessor search-field-window))
  (:documentation "auxilliary buffer attached to all search-buffers to hold searches"))

(defun make-search-field (buffer)
  (let ((field (buf:make-buffer :piece-table)))
    (vico-lib.logging:log :creating-search-field!)
    (dynamic-mixins:ensure-mix field 'search-field) ; destructive
    (setf (slot-value field 'search-buffer) buffer)
    ;; bindings TODO make it a method?
    (setf (gethash :graphic (buf:local-binds field)) #'insert-and-search
          (gethash :alt-s   (buf:local-binds field)) #'search-forwards
          (gethash :alt-n   (buf:local-binds field)) #'search-forwards
          (gethash :alt-b   (buf:local-binds field)) #'search-backwards
          (gethash #\esc    (buf:local-binds field)) #'stop-search)
    field))

(defun fuzzify (term)
  (concatenate 'string
                "("
                (ppcre:regex-replace-all "\\\\ " (ppcre:quote-meta-chars term)
                                         ").*(")
                ")"))

(defun search-forwards (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (handler-case
        (let ((target-point (ui:window-point (search-target buffer))))
          (dotimes (i arg)
            (let ((skip (match-length buffer)))
              (when skip
                (buf:cursor-next-char target-point skip))
              (if-let (match-length
                       (buf:cursor-search-next target-point
                                               (fuzzify (buf:subseq buffer 0))))
                (setf (match-length buffer) match-length)
                (progn
                  (when skip
                    (buf:cursor-prev-char target-point skip))
                  (setf (match-length buffer) nil))))))
      (conditions:vico-bad-index ()))))

(defun insert-and-search (window char arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (vico-lib.commands:insert-char window char arg)
    (let ((target-point (ui:window-point (search-target buffer))))
      (setf (match-length buffer) ; NIL is fine
            (buf:cursor-search-next target-point (fuzzify (buf:subseq buffer 0)))))))

(defun search-backwards (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (dotimes (i arg)
      (buf:cursor-search-prev (ui:window-point (search-target buffer))
                              (fuzzify (buf:subseq buffer 0))))))

(defun stop-search (window arg)
  (declare (ignore arg))
  (with-accessors ((buffer ui:window-buffer)
                   (wx ui:window-x)
                   (wy ui:window-y)
                   (ui ui:window-ui))
      window
    (vico-lib.logging:log :stopped-search)
    ;; TODO maybe we could use a stack and do this a bit smarter
    (setf (search-target buffer) nil
          (match-length buffer) nil
          (ui:focused-window ui) (find (search-buffer buffer)
                                       (ui:windows ui)
                                       :key #'ui:window-buffer))
    (removef (ui:windows ui) window)))

;;; search buffer

(defclass search-buffer (buf:buffer)
  ((symbol-at-point :initform ""
                    :type string
                    :accessor symbol-at-point)
   (search-field :type search-field
                 :accessor search-field)))


(defparameter *search-window-width* 20)
(defun start-search (window arg)
  (declare (ignore arg))
  (with-accessors ((buffer ui:window-buffer)
                   (wx ui:window-x)
                   (ww ui:window-width)
                   (wy ui:window-y)
                   (ui ui:window-ui))
      window
    (let ((field (search-field buffer)))
      (or (search-target field)
          (let ((search-win (ui:make-window ui
                                            (- (+ wx ww) *search-window-width*) 0
                                            *search-window-width* 3
                                            :buffer (search-field buffer)
                                            :floating t
                                            :line-numbers nil :show-status nil)))
            (push search-win (ui:windows ui))
            (setf (search-target field) window
                  (search-field-window field) search-win)))
      (setf (ui:focused-window ui) (search-field-window field)))))

(defmethod update-instance-for-different-class :after
    ((previous buf:buffer) (buffer search-buffer) &rest initargs)
  (declare (ignore initargs))
  (setf (search-field buffer) (make-search-field buffer)
        (gethash :alt-n (buf:local-binds buffer)) #'search-next-word
        (gethash :alt-p (buf:local-binds buffer)) #'search-prev-word
        (gethash :alt-s (buf:local-binds buffer)) #'start-search))

(defmethod buf:close-buffer :after ((buffer search-buffer))
  (buf:close-buffer (search-field buffer)))

(defmethod ui:buffer-styles-for-window append ((buffer search-buffer)
                                               start end window)
  (let (search-term
        spans)
    (flet ((add-span (start-cursor end-cursor
                      &optional (style (hl:make-style :bg #x073642 :fg #x2aa198)))
             (push (make-instance 'ui:style-span
                                  :start start-cursor
                                  :end end-cursor
                                  :style style)
                   spans)))
      ;;(declare (dynamic-extent #'add-span))
      (if (search-target (search-field buffer))
          (let ((term (buf:subseq (search-field buffer) 0)))
            (when (plusp (length term))
              (setf search-term (fuzzify term))))
          (when-let (word (word-at-point (ui:window-point window)))
            (setf word (concatenate 'string "\\b" word "\\b")
                  (symbol-at-point buffer) word
                  search-term word)))
      ;; search and highlight if there's something to be found
      (when search-term
        (loop
          :with it = (buf:copy-cursor start)
          :with left = (buf:cursor- end start)
          :for old = (buf:index-at it)
          :do (multiple-value-bind (length reg-starts reg-ends)
                  (buf:cursor-search-next it search-term left)
                (if length
                    (progn
                      (when (plusp (length reg-starts))
                        (loop :for s :across reg-starts
                              :for e :across reg-ends
                              :do (add-span ; TODO these could just be indices
                                   (buf:cursor-next-char (buf:copy-cursor it) s)
                                   (buf:cursor-next-char (buf:copy-cursor it) e)
                                   (hl:make-style :bg #x073642 :fg #xd33682
                                                  :italicp t))))
                      ;; note cursor is destructively updated
                      (if (and (buf:cursor= it (ui:window-point window))
                               (search-target (search-field buffer)))
                          (add-span (buf:copy-cursor it)
                                    (buf:copy-cursor (buf:cursor-next-char it length))
                                    (hl:make-style :bg #x073642 :fg #x859900
                                                   :italicp t))
                          (add-span (buf:copy-cursor it)
                                    (buf:copy-cursor (buf:cursor-next-char it length))))
                      (decf left (- (buf:index-at it) old)))
                    (return spans))))))))
