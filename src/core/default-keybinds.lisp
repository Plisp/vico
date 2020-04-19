(in-package :vico-core.key-event)

(eval-when (:load-toplevel)
  (setf *default-keybinds*
        (list (cons :control-l (lambda (window)
                                 (declare (ignore window))
                                 :force-redisplay))

              (cons :control-c (lambda (window)
                                 (ui:quit (ui:window-ui window))
                                 (ev:quit-editor-loop ev:*editor*)))

              (cons :control-y (lambda (window)
                                 (let ((top-line (ui:top-line window)))
                                   (unless (< (- (buf:line-at top-line) ev:*editor-arg*)
                                              1)
                                     (buf:cursor-prev-line top-line ev:*editor-arg*)))))

              (cons :control-e (lambda (window)
                                 (let ((top-line (ui:top-line window)))
                                   (unless (> (+ (buf:line-at top-line) ev:*editor-arg*)
                                              (buf:line-count (ui:window-buffer window)))
                                     (buf:cursor-next-line top-line ev:*editor-arg*)))))

              (cons :page-up (lambda (window)
                               (let ((top-line (ui:top-line window))
                                     (delta (1- (ui:window-height window)))
                                     (cap 1))
                                 (buf:cursor-move-to-line top-line
                                                          (max cap
                                                               (- (buf:line-at top-line)
                                                                  delta))))))

              (cons :page-down (lambda (window)
                                 (let ((top-line (ui:top-line window))
                                       (delta (1- (ui:window-height window)))
                                       (cap (buf:line-count (ui:window-buffer window))))
                                   (buf:cursor-move-to-line top-line
                                                            (min cap
                                                                 (+ (buf:line-at top-line)
                                                                    delta)))))))))
