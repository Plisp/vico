;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  TUI entry point and further definitions
;;
;;

(in-package :vico-term)

(defun main (filename)
  (let* (;(filename (or (first (uiop:command-line-arguments)) (return-from main)))
         (*editor* (make-instance 'editor))
         (terminal-dimensions (term:get-terminal-dimensions))
         (tui (make-instance 'tui :width  (cdr terminal-dimensions)
                                  :height (car terminal-dimensions)))
         (initial-buffer (make-instance 'vico-core.standard-buffer:standard-buffer
                                        :initial-file filename))
         (initial-window (ui:make-window tui 1 1
                                         (cdr terminal-dimensions)
                                         (car terminal-dimensions)
                                         :buffer initial-buffer)))
    (setf (ui:focused-window tui) initial-window)
    (push initial-window (ui:windows tui))
    (push initial-buffer (buffers *editor*))
    (setf (slot-value tui 'ui:ui-thread)
          (bt:make-thread (lambda ()
                            (ui:start tui))
                          :name "tui thread"
                          :initial-bindings `((*editor* . ,*editor*))))
    (unwind-protect
         (start-editor-loop *editor*)
      (when (bt:thread-alive-p (ui:ui-thread tui))
        (ui:quit tui)
        (bt:join-thread (ui:ui-thread tui))))))
