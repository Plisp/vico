;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  TUI entry point and further definitions
;;
;;

(in-package :vico-term)

(defun main (filename)
  #+slynk (setf *editor* (make-instance 'editor))
  (with-open-file (file-stream filename :element-type '(unsigned-byte 8))
    (let* (;(filename (or (first (uiop:command-line-arguments)) (return-from main)))
           ;; sly thread messes up signal handling, will work with standalone executable
           #-slynk (*editor* (make-instance 'editor))
           (terminal-dimensions (term:get-terminal-dimensions))
           (tui (make-instance 'tui :width  (cdr terminal-dimensions)
                                    :height (car terminal-dimensions)))
           (initial-buffer
             (buf:make-buffer :piece-table ;:initial-stream file-stream
                              :initial-contents (read-file-into-byte-vector filename))
             ;; (make-instance 'vico-core.standard-buffer:standard-buffer
             ;;                :initial-stream file-stream)
             )
           (initial-window (ui:make-window tui 0 0
                                           (cdr terminal-dimensions)
                                           (car terminal-dimensions)
                                           :buffer initial-buffer)))
      (setf (ui:focused-window tui) initial-window)
      (push initial-window (ui:windows tui))
      (push initial-buffer (buffers *editor*))
      (setf (ui:ui-thread tui)
            (bt:make-thread (lambda ()
                              (ui:start tui))
                            :name "tui thread"
                            :initial-bindings `((*editor* . ,*editor*))))
      (unwind-protect
           (start-editor-loop *editor*)
        (buf:close-buffer initial-buffer)
        #+slynk (setf *editor* nil)
        (when (bt:thread-alive-p (ui:ui-thread tui))
          (ui:quit tui)
          (bt:join-thread (ui:ui-thread tui)))))))
