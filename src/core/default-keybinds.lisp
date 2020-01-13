(in-package :vico-core.key-event)

(eval-when (:load-toplevel)
  (setf *default-keybinds*
        (list (cons :c-l (lambda (window)
                           (declare (ignore window))
                           :force-redisplay))

              (cons :c-c (lambda (window)
                           (ui:quit (ui:window-ui window))
                           (ev:quit-editor-loop ev:*editor*)))

              (cons :c-e (lambda (window)
                           (setf (ui:top-line window)
                                 (min (+ (ui:top-line window) ev:*editor-arg*)
                                      (buffer:line-count (ui:window-buffer window))))
                           (setf ev:*editor-arg* 1)))

              (cons :c-y (lambda (window)
                           (setf (ui:top-line window)
                                 (max (- (ui:top-line window) ev:*editor-arg*)
                                      1))
                           (setf ev:*editor-arg* 1))))))
