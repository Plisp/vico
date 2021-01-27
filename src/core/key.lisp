(defpackage vico-core.key-event
  (:use :cl :alexandria)
  (:local-nicknames (:conditions :vico-core.conditions)
                    (:buf :vico-core.buffer)
                    (:ev :vico-core.evloop)
                    (:ui :vico-core.ui))
  (:export #:key-event #:make-key-event
           #:key-name #:key-window
           #:*default-keybinds*))
(in-package :vico-core.key-event)

(defstruct (key-event (:conc-name key-))
  (name :null)
  window)

(defvar *default-keybinds* nil)

(defmethod ev:handle-event ((event key-event))
  (let* ((window (key-window event))
         (key (key-name event))
         ;; (buf:keybinds (ui:window-buffer window))
         )
    ;;(print key) (force-output)
    ;; "self-insert-command"
    (if-let ((binding (assoc-value *default-keybinds* key)))
      (progn
        (buf:end-undo-group (ui:window-buffer window))
        (funcall binding window))
      (cond ((and (characterp key)
                  (or (graphic-char-p key)
                      (char= key #\newline)
                      (char= key #\return)
                      (char= key #\tab)))
             (buf:begin-undo-group (ui:window-buffer window))
             (buf:insert-at (ui:window-point window) (string key)))
            ((eq key #\rubout)
             (with-accessors ((point ui:window-point)
                              (buffer ui:window-buffer))
                 window
               (buf:begin-undo-group buffer)
               (let ((delete-end (buf:index-at point)))
                 (unless (zerop delete-end)
                   (buf:move-cursor-graphemes* (ui:window-point window) -1)
                   (setf (ui:window-point-column window) nil)
                   (buf:delete-at point (- delete-end (buf:index-at point)))))))
            ((eq key :control-d)
             (with-accessors ((point ui:window-point)
                              (buffer ui:window-buffer))
                 window
               (buf:begin-undo-group buffer)
               (unless (= (buf:index-at point) (buf:size buffer))
                 (let ((start (buf:index-at point)))
                   (buf:move-cursor-graphemes* (ui:window-point window) 1)
                   (setf (ui:window-point-column window) nil)
                   (let ((delete-end (buf:index-at point)))
                     (buf:move-cursor-to point start)
                     (buf:delete-at point (- delete-end (buf:index-at point))))))))))
    (setf ev:*editor-arg* 1)
    (ui:redisplay (ui:window-ui window))))
