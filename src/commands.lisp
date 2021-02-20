(defpackage :vico-lib.commands
  (:use :cl)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:ui :vico-core.ui)
                    (:ed :vico-core.editor)
                    (:log :vico-lib.logging))
  (:export #:next-char
           #:prev-char
           #:next-line
           #:prev-line
           #:start-of-line
           #:end-of-line
           #:next-word
           #:prev-word
           #:scroll-up
           #:scroll-down
           #:page-up
           #:page-down

           #:search-next-occurence
           #:search-prev-occurence

           #:insert-char
           #:delete-char
           #:delete-char-backwards
           #:undo
           #:redo

           #:save-file
           #:editor-quit
           ))
(in-package :vico-lib.commands)

;;; motion TODO merge commands, allow more elaborate argument provision

;; TODO improve performance on long lines

(defun next-char (window arg)
  (buf:move-cursor-graphemes* (ui:window-point window) arg)
  (setf (ui:window-point-column window) nil))

(defun prev-char (window arg)
  (buf:move-cursor-graphemes* (ui:window-point window) (- arg))
  (setf (ui:window-point-column window) nil))

(defun next-word (window arg)
  (dotimes (i arg)
    (let ((point (ui:window-point window)))
      (alexandria:when-let (boundary (buf:cursor-search-next point "\\w+"))
        (buf:cursor-next-char point boundary))))
  (setf (ui:window-point-column window) nil))

(defun prev-word (window arg)
  (dotimes (i arg)
    (let ((point (ui:window-point window)))
      (buf:cursor-search-prev point "^|\\w+")))
  (setf (ui:window-point-column window) nil))

(defun next-line (window arg)
  (buf:move-cursor-lines* (ui:window-point window) arg)
  (ui:window-point-to-max-column window))

(defun prev-line (window arg)
  (buf:move-cursor-lines* (ui:window-point window) (- arg))
  (ui:window-point-to-max-column window))

(defun start-of-line (window arg)
  (declare (ignore arg))
  (buf:cursor-bol (ui:window-point window))
  (setf (ui:window-point-column window) 0))

(defun end-of-line (window arg)
  (declare (ignore arg))
  (buf:cursor-eol (ui:window-point window))
  (setf (ui:window-point-column window) nil))

(defun scroll-up (window arg)
  (ui:scroll-window window (* 2 (- arg))))

(defun scroll-down (window arg)
  (ui:scroll-window window (* 2 arg)))

(defun page-up (window arg)
  (ui:scroll-window window (* (1- (ui:window-height window)) (- arg))))

(defun page-down (window arg)
  (ui:scroll-window window (* (1- (ui:window-height window)) arg)))

;; TODO should be buffer local binding
(defun search-next-occurence (window arg)
  (with-accessors ((buffer ui:window-buffer)
                   (point ui:window-point))
      window
    (when (typep buffer 'vico-lib.keyword-highlighting:keyword-highlighting-buffer)
      (dotimes (i arg)
        (buf:cursor-next-char point)
        (unless (buf:cursor-search-next
                 point
                 (vico-lib.keyword-highlighting:symbol-at-point buffer))
          (buf:cursor-prev-char point))))))

(defun search-prev-occurence (window arg)
  (with-accessors ((buffer ui:window-buffer)
                   (point ui:window-point))
      window
    (when (typep buffer 'vico-lib.keyword-highlighting:keyword-highlighting-buffer)
      (dotimes (i arg)
        (buf:cursor-search-prev
         point (vico-lib.keyword-highlighting:symbol-at-point buffer))))))

;;; editing

(defun insert-char (window char arg)
  (buf:begin-undo-group (ui:window-buffer window))
  (buf:insert-at (ui:window-point window) (make-string arg :initial-element char)))

(defun delete-char-backwards (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (buf:begin-undo-group buffer)
    (let ((delete-end (buf:index-at point)))
      (unless (zerop delete-end)
        (buf:move-cursor-graphemes* (ui:window-point window) (- arg))
        (let ((bytes (- delete-end (buf:index-at point))))
         (decf (ui:window-point-column window)
               (ui:window-string-width window (buf:subseq-at point bytes)))
         (buf:delete-at point bytes))))))

(defun delete-char (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (buf:begin-undo-group buffer)
    (unless (= (buf:index-at point) (buf:size buffer))
      (let ((start (buf:index-at point)))
        (buf:move-cursor-graphemes* (ui:window-point window) arg)
        (let ((delete-end (buf:index-at point)))
          (buf:move-cursor-to point start)
          (buf:delete-at point (- delete-end (buf:index-at point))))))))

(defun undo (window arg)
  (declare (ignore arg))
  (or (buf:undo (ui:window-buffer window))
      (log:log "start-of-history!")))

(defun redo (window arg)
  (declare (ignore arg))
  (or (buf:redo (ui:window-buffer window))
      (log:log "end-of-history!")))

;;; misc

(defun save-file (window arg)
  (declare (ignore arg))
  (let* ((buffer (ui:window-buffer window))
         (tmp (concatenate 'string (buf:filename buffer) "~")))
    (with-open-file (s tmp
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      (buf:write-to-octet-stream buffer s))
    (rename-file tmp (buf:filename buffer))))

(defun editor-quit (window arg)
  (declare (ignore window arg))
  (log:log "exiting-event-loop...")
  (ed:quit-editor-loop ed:*editor*))
