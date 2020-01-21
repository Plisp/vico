(in-package :vico-core.key-event)

(eval-when (:load-toplevel)
  (setf *default-keybinds*
        (list (cons :control-l (lambda (window)
                                 (declare (ignore window))
                                 :force-redisplay))

              (cons :control-c (lambda (window)
                                 (ui:quit (ui:window-ui window))
                                 (ev:quit-editor-loop ev:*editor*)))

              (cons :control-e (lambda (window)
                                 (setf (ui:top-line window)
                                       (min (+ (ui:top-line window) ev:*editor-arg*)
                                            (buffer:line-count (ui:window-buffer window))))
                                 (setf ev:*editor-arg* 1)))

              (cons :control-y (lambda (window)
                                 (setf (ui:top-line window)
                                       (max (- (ui:top-line window) ev:*editor-arg*)
                                            1))
                                 (setf ev:*editor-arg* 1)))

              (cons :page-up (lambda (window)
                               (setf (ui:top-line window)
                                     (max (- (ui:top-line window)
                                             (1- (ui:window-height window)))
                                          1))
                               (setf ev:*editor-arg* 1)))

              (cons :page-down (lambda (window)
                                 (setf (ui:top-line window)
                                       (min (+ (ui:top-line window)
                                               (1- (ui:window-height window)))
                                            (buffer:line-count (ui:window-buffer window))))
                                 (setf ev:*editor-arg* 1))))))
