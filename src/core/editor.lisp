;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editor event loop (main)
;;
;;

(defpackage :vico-core.editor
  (:use :cl)
  (:local-nicknames )
  (:export ))
(in-package :vico-core.editor)


;; ;; TODO may need to switch to priority queue for command priorities
;; (defmethod start-editor-loop ((editor editor))
;;   (catch 'quit-editor-loop
;;     (loop
;;       (block handling-command
;;         (restart-case
;;             (multiple-value-bind (command context)
;;                 (read-command (command-queue editor))
;;               (handle-command command context))
;;           (never-gonna-give-you-up ()
;;             (return-from handling-command)))))))

;; ;; must be called from within the dynamic extent of `start-editor-loop`
;; (defmethod quit-editor-loop ((editor editor))
;;   (throw 'quit-editor-loop :quit))
