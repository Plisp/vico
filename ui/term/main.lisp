;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  TUI entry point and further definitions
;;
;;

(in-package :vico-term)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(defun %main (terminal-dimensions tui filename)
  (with-open-file (file-stream filename :element-type '(unsigned-byte 8))
    (let* ((initial-buffer
             (buf:make-buffer :piece-table :initial-stream file-stream
                              ;;:initial-contents (read-file-into-string filename)
                              ))
           (initial-window (ui:make-window tui 0 0
                                           (cdr terminal-dimensions)
                                           (car terminal-dimensions)
                                           :buffer initial-buffer))
           ;; (initial-split (ui:make-window tui 0 0
           ;;                                 (cdr terminal-dimensions)
           ;;                                 (car terminal-dimensions)
           ;;                                 :buffer initial-buffer
           ;;                                 :line-numbers nil))
           )
      (dynamic-mixins:ensure-mix initial-buffer 'vico-lib.search:search-buffer)
      (setf (ui:focused-window tui) initial-window)
      ;;(setf (ui:layout tui) `(,initial-window (,initial-split . 1/3)))
      (setf (ui:layout tui) (list initial-window))
      (vico-term.impl::layout-windows tui)
      (push initial-window (ui:windows tui))
      ;;(push initial-split (ui:windows tui))
      (push initial-buffer (ed:buffers ed:*editor*))
      (setf (ui:ui-thread tui)
            (bt:make-thread (lambda ()
                              (ui:start tui))
                            :name "tui thread"
                            :initial-bindings `((ed:*editor* . ,ed:*editor*))))
      (sb-sprof:start-profiling :threads (list (ui:ui-thread tui))
                                :sample-interval 0.001)
      (unwind-protect
           (ed:start-editor-loop ed:*editor*)
        (buf:close-buffer initial-buffer)
        (setf ed:*editor* nil)
        (sb-sprof:stop-profiling)
        (when (bt:thread-alive-p (ui:ui-thread tui))
          (ui:quit tui)
          (bt:join-thread (ui:ui-thread tui)))))))

(defun main ()
  (let* ((filename (or (first (uiop:command-line-arguments))
                       (progn (format t "nothing to do tooday?~%")
                              (return-from main))))
         ;; sly thread messes up signal handling, works with standalone executable
         (ed:*editor* (make-instance 'ed:editor))
         (terminal-dimensions (term:terminal-dimensions))
         (tui (make-instance 'tui :columns (cdr terminal-dimensions)
                                  :rows (car terminal-dimensions))))
    (%main terminal-dimensions tui filename)))

(defun dmain (filename)
  (setf ed:*editor* (make-instance 'ed:editor))
  (let* ((terminal-dimensions (term:terminal-dimensions))
         (tui (make-instance 'tui :columns (cdr terminal-dimensions)
                                  :rows (car terminal-dimensions))))
    (%main terminal-dimensions tui filename)))
