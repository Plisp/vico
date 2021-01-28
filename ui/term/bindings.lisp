(in-package :vico-term.bindings)

;; TODO
(defparameter *command-arg* 1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *keybinds*
    (apply #'alist-hash-table
           (list
            (cons :control-q
                  (lambda (window)
                    (declare (ignore window))
                    (ed:quit-editor-loop ed:*editor*)
                    (log:log :editor-loop-exited)))

            (cons :control-a
                  (lambda (window) ;TODO maybe expose GOAL-COLUMN type thing?
                    (buf:cursor-bol (ui:window-point window))
                    (setf (ui:window-point-column window) nil)))

            (cons :control-e
                  (lambda (window)
                    (buf:cursor-eol (ui:window-point window))
                    (setf (ui:window-point-column window) nil)))

            (cons :control-s
                  (lambda (window)
                    (let ((buffer (ui:window-buffer window)))
                      (with-open-file (s (concatenate 'string (buf:filename buffer) "~")
                                         :direction :output
                                         :if-exists :supersede
                                         :element-type '(unsigned-byte 8))
                        (buf:write-to-octet-stream buffer s)))))

            (cons :control-z
                  (lambda (window)
                    (or (buf:undo (ui:window-buffer window))
                        (log:log :start-of-history!))))

            (cons :control-r
                  (lambda (window)
                    (or (buf:redo (ui:window-buffer window))
                        (log:log :end-of-history!))))

            (cons :alt-b
                  (lambda (window)
                    (let ((point (ui:window-point window)))
                      (buf:cursor-search-prev point "^|\\w+"))
                    (setf (ui:window-point-column window) nil)))

            (cons :alt-f
                  (lambda (window)
                    (let ((point (ui:window-point window)))
                      (when-let (length (buf:cursor-search-next point "\\w+"))
                        (buf:cursor-next-char point length)))
                    (setf (ui:window-point-column window) nil)))

            (cons :control-w
                  (lambda (window)
                    (buf:cursor-search-prev (ui:window-point window) "gga")))
            (cons :control-c
                  (lambda (window)
                    (buf:cursor-search-next (ui:window-point window) "g.*a")))

            (cons :control-p
                  (lambda (window)
                    (buf:move-cursor-lines* (ui:window-point window) (- *command-arg*))
                    (ui:window-point-to-max-column window)))

            (cons :control-n
                  (lambda (window)
                    (buf:move-cursor-lines* (ui:window-point window) *command-arg*)
                    (ui:window-point-to-max-column window)))

            (cons :control-b
                  (lambda (window)
                    (buf:move-cursor-graphemes* (ui:window-point window) -1)
                    (setf (ui:window-point-column window) nil)))

            (cons :control-f
                  (lambda (window)
                    (buf:move-cursor-graphemes* (ui:window-point window) 1)
                    (setf (ui:window-point-column window) nil)))

            (cons :control-y
                  (lambda (window)
                    (ui:scroll-window window (* 2 (- *command-arg*)))))

            (cons :control-t
                  (lambda (window)
                    (ui:scroll-window window (* 2 *command-arg*))))

            (cons :page-up
                  (lambda (window)
                    (ui:scroll-window window (- (1- (ui:window-height window))))))

            (cons :page-down
                  (lambda (window)
                    (ui:scroll-window window (1- (ui:window-height window))))))
           #+(or sbcl ecl) (list :synchronized t)
           #+ccl nil)))

(defun insert-command (window string arg)
  (buf:begin-undo-group (ui:window-buffer window))
  (dotimes (i arg)
    (buf:insert-at (ui:window-point window) string)))

(defun delete-char-backwards (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (buf:begin-undo-group buffer)
    (let ((delete-end (buf:index-at point)))
      (unless (zerop delete-end)
        (buf:move-cursor-graphemes* (ui:window-point window) (- arg))
        (setf (ui:window-point-column window) nil)
        (buf:delete-at point (- delete-end (buf:index-at point)))))))

(defun delete-char (window arg)
  (with-accessors ((point ui:window-point)
                   (buffer ui:window-buffer))
      window
    (buf:begin-undo-group buffer)
    (unless (= (buf:index-at point) (buf:size buffer))
      (let ((start (buf:index-at point)))
        (buf:move-cursor-graphemes* (ui:window-point window) arg)
        (setf (ui:window-point-column window) nil)
        (let ((delete-end (buf:index-at point)))
          (buf:move-cursor-to point start)
          (buf:delete-at point (- delete-end (buf:index-at point))))))))

(defun lookup-binding (tui key &optional (window (ui:focused-window tui)))
  (if-let ((binding (gethash key *keybinds*)))
    (progn
      (buf:end-undo-group (ui:window-buffer window))
      (list binding window))
    ;; "self-insert-command"
    (cond ((and (characterp key)
                (or (graphic-char-p key)
                    (char= key #\newline)
                    (char= key #\return)
                    (char= key #\tab)))
           (list #'insert-command window (string key) *command-arg*))
          ((eq key #\rubout)
           (list #'delete-char-backwards window *command-arg*))
          ((eq key :control-d)
           (list #'delete-char window *command-arg*)))))
