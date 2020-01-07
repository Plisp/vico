(in-package :vico-core.key-event)

(setf *default-keybinds*
      (list (cons :c-l (lambda (window)
                         :force-redisplay))

            (cons :c-c (lambda (window)
                         (ui:quit (ui:window-ui window))
                         (ev:quit-editor-loop ev:*editor*)))

            (cons :c-e (lambda (window)
                         (when (< (+ (ui:top-line window) ev:*editor-arg*)
                                  (buffer:line-count (ui:window-buffer window)))
                           (incf (ui:top-line window) ev:*editor-arg*))
                         (setf ev:*editor-arg* 1)))

            (cons :c-y (lambda (window)
                         (when (> (ui:top-line window) ev:*editor-arg*)
                           (decf (ui:top-line window) ev:*editor-arg*))
                         (setf ev:*editor-arg* 1)))))
