;;;; entry point and further definitions

(in-package :vico-term)

(defun main (filename)
  (let (;; rebind to make terminfo functions work
        #+(or cmu sbcl)
        (*terminal-io* *standard-output*))
    (let* ((initial-buffer
             (make-buffer 'piece-table-buffer :initial-contents (text-file-to-string filename)))
           (dimensions (term:get-terminal-dimensions))
           (tui (make-instance 'tui))
           (initial-window
             (ui:make-window tui 1 1 (cdr dimensions) (car dimensions) :buffer initial-buffer))
           (editor (make-instance 'editor :buffers (list initial-buffer))))
      (push tui (ui-list editor))
      (push initial-window (ui:windows tui))
      (let (orig-termios)
        (unwind-protect
             (progn
               (setf orig-termios (term:setup-terminal-input))
               (ti:set-terminal (uiop:getenv "TERM"))
               (ti:tputs ti:clear-screen) ;TODO line wrap
               (loop :for event = (term:read-terminal-event)
                     :until (and (characterp event) (char= event #\Etx))
                     :do (ui:redisplay-window initial-window))
               (deletef (ui-list editor) tui))
          (when orig-termios
            (term:restore-terminal-input orig-termios)))))))
