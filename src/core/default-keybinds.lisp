(in-package :vico-core.key-event)

(eval-when (:load-toplevel)
  (setf *default-keybinds*
        (list
         (cons :control-l (lambda (window)
                            (declare (ignore window))
                            :force-redisplay))

         (cons :control-w (lambda (window)
                            (declare (ignore window))
                            (ev:quit-editor-loop ev:*editor*)))

         (cons :control-d (lambda (window)
                            (if (>= (buf:index-at (ui:window-point window))
                                    (- (buf:size (ui:window-buffer window)) 2))
                                (ev:log-event "trying to delete at buffer end!")
                                (buf:delete-at (ui:window-point window) ev:*editor-arg*))))

         (cons :backspace (lambda (window)
                            (unless (zerop (buf:index-at (ui:window-point window)))
                              (ui:move-point window (- ev:*editor-arg*))
                              (buf:delete-at (ui:window-point window) ev:*editor-arg*))))
         ;;cursor
         (cons :control-a (lambda (window)
                            (buf:cursor-bol (ui:window-point window))))

         (cons :control-e (lambda (window)
                            (buf:cursor-eol (ui:window-point window))))

         (cons :control-s (lambda (window)
                            (ui:move-point window)
                            (unless (buf:cursor-search-next (ui:window-point window)
                                                            "randomized")
                              (ui:move-point window -1))
                            ;;(buf:cursor-search-prev (ui:window-point window) "randomized")
                            ))

         (cons :alt-b (lambda (window)
                        (loop :for iterations :from 1
                              :do (ui:move-point window -1)
                              :until (and (> iterations 1)
                                          (not (alphanumericp
                                                (buf:char-at (ui:window-point window)))))
                              :finally (ui:move-point window))))

         (cons :alt-f (lambda (window)
                        (loop :for iterations :from 1
                              :until (and (> iterations 1)
                                          (not (alphanumericp
                                                (buf:char-at (ui:window-point window)))))
                              :do (ui:move-point window))))

         (cons :control-p (lambda (window)
                            (ui:move-point-lines window (- ev:*editor-arg*))))

         (cons :control-n (lambda (window)
                            (ui:move-point-lines window ev:*editor-arg*)))

         (cons :control-b (lambda (window)
                            (ui:move-point window (- ev:*editor-arg*))))

         (cons :control-f (lambda (window)
                            (ui:move-point window ev:*editor-arg*)))
         ;;scroll
         (cons :control-y (lambda (window)
                            (ui:scroll-window window (* 2 (- ev:*editor-arg*)))))

         (cons :control-t (lambda (window)
                            (ui:scroll-window window (* 2 ev:*editor-arg*))))

         (cons :page-up (lambda (window)
                          (ui:scroll-window window (- (1- (ui:window-height window))))))

         (cons :page-down (lambda (window)
                            (ui:scroll-window window (1- (ui:window-height window))))))))
