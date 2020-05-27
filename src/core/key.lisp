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
         (val (when-let ((binding (assoc-value ;; (buf:keybinds (ui:window-buffer window))
                                   *default-keybinds* key)))
                (funcall binding window))))
    ;;(print key) (force-output)
    ;; "self-insert-command"
    (when (and (not val) (characterp key)
               (or (not (< (char-code key) 32))
                   (char= key #\newline))
               (not (<= 127 (char-code key) 160)))
      (assert (buf:cursor-valid-p (ui:window-point window)))
      (buf:insert-at (ui:window-point window) (string key))
      (ui:move-point window 1) ;right sticky?
      (ui:redisplay (ui:window-ui window)))

    (setf ev:*editor-arg* 1)
    (if (eq val :force-redisplay)
        (ui:redisplay (ui:window-ui window) :force-p t)
        (ui:redisplay (ui:window-ui window))) ; feedback is important!
    val))
