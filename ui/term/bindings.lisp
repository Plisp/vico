(in-package :vico-term.bindings)

;; TODO this can be more useful
(defparameter *command-arg* 1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *keybinds*
    (apply #'alist-hash-table
           (list
            (cons :control-q 'cmd:editor-quit)
            (cons :control-s 'cmd:save-file)
            (cons :control-z 'cmd:undo)
            (cons :control-r 'cmd:redo)
            ;; windowing
            (cons :control-w 'cmd:cycle-focus)
            (cons :control-} 'cmd:split-vertical)
            ;; motions
            (cons :control-f 'cmd:next-char)
            (cons :control-b 'cmd:prev-char)
            (cons :control-n 'cmd:next-line)
            (cons :control-p 'cmd:prev-line)
            (cons :control-a 'cmd:start-of-line)
            (cons :control-e 'cmd:end-of-line)
            (cons :alt-f 'cmd:next-word)
            (cons :alt-b 'cmd:prev-word)
            (cons :right-arrow 'cmd:next-char)
            (cons :left-arrow 'cmd:prev-char)
            (cons :down-arrow 'cmd:next-line)
            (cons :up-arrow 'cmd:prev-line)
            ;; edit
            (cons :control-k 'cmd:delete-line)
            ;; scrolling
            (cons :control-y 'cmd:scroll-up)
            (cons :control-t 'cmd:scroll-down)
            (cons :page-up   'cmd:page-up)
            (cons :page-down 'cmd:page-down))
           #+(or sbcl ecl) (list :synchronized t)
           #+ccl nil)))

(defun graphic-input-p (event)
  (and (characterp event)
       (or (graphic-char-p event)
           (char= event #\newline)
           (char= event #\return)
           (char= event #\tab))))

(defun lookup-binding (tui key &optional (window (ui:focused-window tui)))
  ;; TODO probably use a generic function
  (if (graphic-input-p key) ; "self-insert-command"
      (if-let ((local-bind (gethash :graphic (buf:local-binds (ui:window-buffer window)))))
        (list local-bind window key *command-arg*)
        (list 'cmd:insert-char window key *command-arg*)) ; global to frontend
      (if-let ((local-bind (gethash key (buf:local-binds (ui:window-buffer window)))))
        (list local-bind window *command-arg*)
        (if-let ((binding (gethash key *keybinds*))) ; global to frontend
          (progn
            (buf:end-undo-group (ui:window-buffer window))
            (list binding window *command-arg*)) ; TODO more general protocol
          ;; undo group not reset for these bindings
          (case key
            (#\rubout (list 'cmd:delete-char-backwards window *command-arg*))
            (:control-d (list 'cmd:delete-char window *command-arg*)))))))
