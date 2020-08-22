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
                 (buf:cursor-bol (ui:window-point window))))

         (cons :control-e
               (lambda (window)
                 (buf:cursor-eol (ui:window-point window))))

         (cons :control-s
               (lambda (window)
                 (let ((buffer (ui:window-buffer window)))
                   (with-open-file (s (concatenate 'string
                                                   (namestring (buf:filename buffer))
                                                   "~")
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
                 (loop :for iterations :from 1
                       :do (ui:move-point window -1)
                       :until (and (> iterations 1)
                                   (not (alphanumericp
                                         (buf:char-at (ui:window-point window)))))
                       :finally (ui:move-point window))))

         (cons :alt-f
               (lambda (window)
                 (loop :for iterations :from 1
                       :until (and (> iterations 1)
                                   (not (alphanumericp
                                         (buf:char-at (ui:window-point window)))))
                       :do (ui:move-point window))))

         (cons :control-p
               (lambda (window)
                 (ui:move-point-lines window (- ev:*editor-arg*))))

         (cons :control-n
               (lambda (window)
                 (ui:move-point-lines window ev:*editor-arg*)))

         (cons :control-b
               (lambda (window)
                 (ui:move-point window (- ev:*editor-arg*))))

         (cons :control-f
               (lambda (window)
                 (ui:move-point window ev:*editor-arg*)))

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
