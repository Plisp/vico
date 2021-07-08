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

           #:insert-char
           #:delete-char
           #:delete-char-backwards
           #:delete-line
           #:undo
           #:redo

           #:split-horizontal
           #:split-vertical
           #:cycle-focus

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

;;; editing

(defun insert-char (window char arg)
  (with-accessors ((buffer ui:window-buffer)
                   (point ui:window-point))
      window
    (buf:begin-undo-group buffer)
    (dotimes (i arg)
      (buf:insert-at point (string char))
      (alexandria:when-let (i (position char '(#\( #\[ #\{ #\<) :test #'char=))
        (buf:insert-at point (string
                              (aref #(#\) #\] #\} #\>) i)))
        (buf:cursor-prev point)))))

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

(defun delete-line (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (buf:begin-undo-group buffer)
    (dotimes (i arg)
      (let ((start (buf:index-at point)))
        (unless (= start (buf:size buffer))
          (if (char= #\newline (buf:char-at point))
              (delete-char window 1)
              (progn
                (buf:move-cursor-lines* point 1)
                ;; end of buffer, no trailing newline
                (unless (= (buf:index-at point) (buf:size buffer))
                  (buf:move-cursor* point -1))
                (let* ((delete-end (buf:index-at point))
                       (extent (- delete-end start)))
                  (buf:move-cursor-to point start)
                  (log:log extent)
                  (when (plusp extent)
                    (buf:delete-at point (- delete-end (buf:index-at point))))))))))))

(defun undo (window arg)
  (declare (ignore arg))
  (or (buf:undo (ui:window-buffer window))
      (log:log "start-of-history!")))

(defun redo (window arg)
  (declare (ignore arg))
  (or (buf:redo (ui:window-buffer window))
      (log:log "end-of-history!")))

;;; windowing

(defun split-vertical (window arg)
  (declare (ignore window arg))
  (log:log "splitting window vertically!"))

(defun cycle-focus (window arg)
  (let* ((ui (ui:window-ui window))
         (window-list (ui:windows ui)))
    (dotimes (i arg)
      (let ((pos (position window window-list)))
        (setf (ui:focused-window ui)
              (nth (mod (1+ pos) (length window-list)) window-list))))))

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
