;;;; entry point and further definitions

(in-package :vico-term)

(defun main (filename)
  (let (;; rebind to make terminfo library work
        #+(or cmu sbcl)
        (*terminal-io* *standard-output*))
    (let* ((initial-buffer (make-buffer 'piece-table-buffer
                                        :initial-contents (text-file-to-string filename)))
           (dimensions (atc:dimensions))
           (initial-window
             (make-window 1 1 (cdr dimensions) (car dimensions)
                          :window-buffer initial-buffer))
           (tui (make-instance 'tui :buffers (list initial-buffer))))
      (push tui ui:*frontends*)
      (push initial-window (list-windows))
      (let (orig-termios)
        (unwind-protect
             (progn
               (setf orig-termios (vico-term.util:setup-terminal-input))
               (ti:set-terminal (uiop:getenv "TERM"))
               (ti:tputs ti:clear-screen) ;TODO line wrap
               (loop :for event = (atc:read-event)
                     :until (and event (char= event #\Etx))
                     :do (redisplay-window initial-window))
               (setf ui:*frontends* (delete tui ui:*frontends*)))
          (when orig-termios
            (vico-term.util:restore-terminal-input orig-termios)))))))
