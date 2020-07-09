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

         (cons :control-d (lambda (window) ;TODO end-of-buffer-p?
                            (handler-case
                                (buf:delete-at (ui:window-point window) ev:*editor-arg*)
                              (conditions:vico-bad-index ()
                                (ev:log-event "end of buffer!")))))

         (cons :backspace (lambda (window)
                            (unless (zerop (buf:index-at (ui:window-point window)))
                              (ui:move-point window (- ev:*editor-arg*))
                              (buf:delete-at (ui:window-point window) ev:*editor-arg*))))
         ;;cursor
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
                            (ui:scroll-window window (- ev:*editor-arg*))))

         (cons :control-e (lambda (window)
                            (ui:scroll-window window ev:*editor-arg*)))

         (cons :page-up (lambda (window)
                          (ui:scroll-window window (- (1- (ui:window-height window))))))

         (cons :page-down (lambda (window)
                            (ui:scroll-window window (1- (ui:window-height window))))))))
