(in-package :vico-core.key-event)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-keybinds*
        (list
         (cons :control-l
               (lambda (window)
                 (declare (ignore window))
                 :force-redisplay))

         (cons :control-q
               (lambda (window)
                 (declare (ignore window))
                 (ev:quit-editor-loop ev:*editor*)))

         (cons :control-a
               (lambda (window) ;TODO maybe expose GOAL-COLUMN type thing?
                 (buf:cursor-bol (ui:window-point window))
                 (setf (ui:window-point-column window) :current)))

         (cons :control-e
               (lambda (window)
                 (buf:cursor-eol (ui:window-point window))
                 (setf (ui:window-point-column window) :current)))

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
                     (ev:log-event :start-of-history!))))

         (cons :control-r
               (lambda (window)
                 (or (buf:redo (ui:window-buffer window))
                     (ev:log-event :end-of-history!))))

         (cons :alt-b
               (lambda (window)
                 (let ((point (ui:window-point window)))
                   (buf:cursor-search-prev point "^|\\w+"))
                 (setf (ui:window-point-column window) :current)))

         (cons :alt-f
               (lambda (window)
                 (let ((point (ui:window-point window)))
                   (when-let (length (buf:cursor-search-next point "\\w+"))
                     (buf:cursor-next-char point length)))
                 (setf (ui:window-point-column window) :current)))

         (cons :control-w
               (lambda (window)
                 (buf:cursor-search-prev (ui:window-point window) "gga")))
         (cons :control-c
               (lambda (window)
                 (buf:cursor-search-next (ui:window-point window) "g.*a")))

         (cons :control-p
               (lambda (window)
                 (buf:move-cursor-lines* (ui:window-point window) (- ev:*editor-arg*))
                 (ui:window-point-to-max-column window)))

         (cons :control-n
               (lambda (window)
                 (buf:move-cursor-lines* (ui:window-point window) ev:*editor-arg*)
                 (ui:window-point-to-max-column window)))

         (cons :control-b
               (lambda (window)
                 (buf:move-cursor-graphemes* (ui:window-point window) -1)
                 (setf (ui:window-point-column window) :current)))

         (cons :control-f
               (lambda (window)
                 (buf:move-cursor-graphemes* (ui:window-point window) 1)
                 (setf (ui:window-point-column window) :current)))

         (cons :control-y
               (lambda (window)
                 (ui:scroll-window window (* 2 (- ev:*editor-arg*)))))

         (cons :control-t
               (lambda (window)
                 (ui:scroll-window window (* 2 ev:*editor-arg*))))

         (cons :page-up
               (lambda (window)
                 (ui:scroll-window window (- (1- (ui:window-height window))))))

         (cons :page-down
               (lambda (window)
                 (ui:scroll-window window (1- (ui:window-height window))))))))
