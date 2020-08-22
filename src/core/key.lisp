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
         (val))
    ;;(print key) (force-output)
    ;; "self-insert-command"
    (if-let ((binding (assoc-value *default-keybinds* key)))
      (progn
        (buf:end-undo-group (ui:window-buffer window))
        (setf val (funcall binding window)))
      (cond ((and (characterp key)
                  (or (graphic-char-p key)
                      (char= key #\newline)
                      (char= key #\return)
                      (char= key #\tab)))
             (buf:begin-undo-group (ui:window-buffer window))
             (buf:insert-at (ui:window-point window) (string key)))
            ((eq key :backspace)
             (with-accessors ((point ui:window-point)
                              (buffer ui:window-buffer))
                 window
               (buf:begin-undo-group buffer)
               (let ((delete-end (buf:index-at point)))
                 (unless (zerop delete-end)
                   (ui:move-point window (- ev:*editor-arg*))
                   (buf:delete-at point (- delete-end (buf:index-at point)))))))
            ((eq key :control-d)
             (with-accessors ((point ui:window-point)
                              (buffer ui:window-buffer))
                 window
               (buf:begin-undo-group buffer)
               (unless (= (buf:index-at point) (buf:size buffer))
                 (ui:move-point window ev:*editor-arg*)
                 (let ((delete-end (buf:index-at point)))
                   (ui:move-point window (- ev:*editor-arg*))
                   (buf:delete-at point (- delete-end (buf:index-at point)))))))))

    (setf ev:*editor-arg* 1)
    (if (eq val :force-redisplay)
        (ui:redisplay (ui:window-ui window) :force-p t)
        (ui:redisplay (ui:window-ui window))) ; feedback is important!
    val))
