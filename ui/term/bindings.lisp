(in-package :vico-term.bindings)

;; TODO
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
            (cons :alt-n 'cmd:search-next-occurence)
            (cons :alt-p 'cmd:search-prev-occurence)
            ;; scrolling
            (cons :control-y 'cmd:scroll-up)
            (cons :control-t 'cmd:scroll-down)
            (cons :page-up   'cmd:page-up)
            (cons :page-down 'cmd:page-down))
           #+(or sbcl ecl) (list :synchronized t)
           #+ccl nil)))

(defun lookup-binding (tui key &optional (window (ui:focused-window tui)))
  (if-let ((binding (gethash key *keybinds*)))
    (progn
      (buf:end-undo-group (ui:window-buffer window))
      (list binding window *command-arg*)) ; TODO more general protocol
    (cond ((and (characterp key) ; "self-insert-command"
                (or (graphic-char-p key)
                    (char= key #\newline)
                    (char= key #\return)
                    (char= key #\tab)))
           (list 'cmd:insert-char window key *command-arg*))
          ((eq key #\rubout)
           (list 'cmd:delete-char-backwards window *command-arg*))
          ((eq key :control-d)
           (list 'cmd:delete-char window *command-arg*)))))
