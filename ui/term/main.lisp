;;;;
;;;
;;
;; TUI entry point and further definitions
;;
;;;
;;;;

(in-package :vico-term)

(defun main (filename)
  (let* (;(filename (or (first (uiop:command-line-arguments)) (return-from main)))
         (*editor* (make-instance 'editor))
         (terminal-dimensions (term:get-terminal-dimensions))
         (tui (make-instance 'tui :width  (cdr terminal-dimensions)
                                  :height (car terminal-dimensions)))
         (initial-buffer
           (make-instance 'vico-lib.standard-buffer:standard-buffer
                          :initial-contents (text-file-to-string filename)
                          :inherit-keybinds nil
                          :local-keybinds
                          (pairlis (list vico-term.impl::+c-l+ ; XXX
                                         vico-term.impl::+c-c+
                                         vico-term.impl::+c-e+
                                         vico-term.impl::+c-y+)
                                   (list (lambda (context)
                                           (declare (ignore context))
                                           (bt:interrupt-thread
                                            (ui:ui-thread tui)
                                            (lambda ()
                                              (ui:redisplay-window
                                                (ui:focused-window tui)))))
                                         (lambda (context)
                                           (declare (ignore context))
                                           (ui:quit tui)
                                           (quit-editor-loop))
                                         (lambda (context)
                                           (declare (ignore context))
                                           (bt:interrupt-thread
                                            (ui:ui-thread tui)
                                            (lambda ()
                                              (incf (vico-term.impl::%top-line
                                                     (ui:focused-window tui)))
                                              (ui:redisplay-window
                                                (ui:focused-window tui)))))
                                         (lambda (context)
                                           (declare (ignore context))
                                           (bt:interrupt-thread
                                            (ui:ui-thread tui)
                                            (lambda ()
                                              (when (> (vico-term.impl::%top-line
                                                        (ui:focused-window tui)) 1)
                                                (decf (vico-term.impl::%top-line
                                                       (ui:focused-window tui))))
                                              (ui:redisplay-window
                                                (ui:focused-window tui)))))))))
         (initial-window (ui:make-window tui 1 1
                                         (cdr terminal-dimensions) (car terminal-dimensions)
                                         :buffer initial-buffer)))
    (setf (ui:focused-window tui) initial-window)
    (push initial-window (ui:windows tui))
    (push initial-buffer (buffers *editor*))
    (setf (slot-value vico-term.impl::+c-l+ 'key-window) initial-window
          (slot-value vico-term.impl::+c-c+ 'key-window) initial-window
          (slot-value vico-term.impl::+c-e+ 'key-window) initial-window
          (slot-value vico-term.impl::+c-y+ 'key-window) initial-window)

    (setf (ui:ui-thread tui) (bt:make-thread (lambda () (ui:start tui))
                                             :name "tui thread"
                                             :initial-bindings `((*editor* . ,*editor*))))
    (start-editor-loop)
    (bt:join-thread (ui:ui-thread tui))))
