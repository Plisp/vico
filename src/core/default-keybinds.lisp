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

         (cons :control-d
               (lambda (window)
                 (with-accessors ((point ui:window-point)
                                  (buffer ui:window-buffer))
                     window
                   (unless (= (buf:index-at point) (buf:size buffer))
                     (ui:move-point window ev:*editor-arg*)
                     (let ((delete-end (buf:index-at point)))
                       (ui:move-point window (- ev:*editor-arg*))
                       (buf:delete-at point (- delete-end (buf:index-at point))))))))

         (cons :backspace
               (lambda (window)
                 (with-accessors ((point ui:window-point))
                     window
                   (let ((delete-end (buf:index-at point)))
                     (unless (zerop delete-end)
                       (ui:move-point window (- ev:*editor-arg*))
                       (buf:delete-at point (- delete-end (buf:index-at point))))))))

         (cons :control-a
               (lambda (window) ;TODO maybe expose GOAL-COLUMN type thing?
                 (buf:cursor-bol (ui:window-point window))))

         (cons :control-e
               (lambda (window)
                 (buf:cursor-eol (ui:window-point window))))

         (cons :control-s
               (lambda (window)
                 (ui:move-point window)
                 (unless (buf:cursor-search-next (ui:window-point window) "randomized")
                   (ui:move-point window -1))
                 ;;(buf:cursor-search-prev (ui:window-point window) "randomized")
                 ))

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
