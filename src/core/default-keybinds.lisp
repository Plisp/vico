(in-package :vico-core.key-event)

(eval-when (:load-toplevel)
  (setf *default-keybinds*
        (list
         (cons :control-l (lambda (window)
                            (declare (ignore window))
                            :force-redisplay))

         (cons :control-c (lambda (window)
                            (declare (ignore window))
                            (ev:quit-editor-loop ev:*editor*)))
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
                            (let ((top-line (ui:window-top-line window)))
                              (unless (< (- (buf:line-at top-line) ev:*editor-arg*)
                                         1)
                                (ui:scroll-window window (- ev:*editor-arg*))))))

         (cons :control-e (lambda (window)
                            (let ((top-line (ui:window-top-line window)))
                              (unless (> (+ (buf:line-at top-line) ev:*editor-arg*)
                                         (buf:line-count (ui:window-buffer window)))
                                (ui:scroll-window window ev:*editor-arg*)))))

         (cons :control-d (lambda (window)
                            (buf:erase-at (ui:window-point window) ev:*editor-arg*)
                            (assert (buf:cursor-valid-p (ui:window-point window)))))

         (cons :backspace (lambda (window)
                            (ui:move-point window (- ev:*editor-arg*))
                            (buf:erase-at (ui:window-point window) ev:*editor-arg*)
                            (assert (buf:cursor-valid-p (ui:window-point window)))))

         (cons :page-up (lambda (window)
                          (let ((top-line (buf:line-at (ui:window-top-line window)))
                                (delta (1- (ui:window-height window)))
                                (cap 1))
                            (ui:scroll-window window (- (max cap (- top-line delta))
                                                        top-line)))))

         (cons :page-down (lambda (window)
                            (let ((top-line (buf:line-at (ui:window-top-line window)))
                                  (delta (1- (ui:window-height window)))
                                  (cap (buf:line-count (ui:window-buffer window))))
                              (ui:scroll-window window (- (min cap (+ top-line delta))
                                                          top-line))))))))
