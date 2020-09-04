(defpackage :vico-core.syntax-highlighting
  (:use :cl)
  (:export #:red #:green #:blue

           #:make-style #:style
           #:fg #:bg
           #:boldp #:italicp #:reversep #:underlinep

           #:style-difference
           #:syntax-style
           #:*default-style*

           #:cl-lexer #:todo-lexer))
(in-package :vico-core.syntax-highlighting)

;;; defs

(defstruct (style (:conc-name nil))
  (fg nil :type (or null (integer #x000000 #xffffff)))
  (bg nil :type (or null (integer #x000000 #xffffff)))
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (reversep nil :type boolean)
  (underlinep nil :type boolean))

(defun red (color) (ldb (byte 8 16) color))
(defun green (color) (ldb (byte 8 8) color))
(defun blue (color) (ldb (byte 8 0) color))

(defun style-difference (a b)
  (let ((fga (fg a))
        (fgb (fg b))
        (bga (bg a))
        (bgb (bg b))
        differences)
    (unless (or (and (null fga) (null fgb))
                (and fga fgb
                     (= (red fga) (red fgb))
                     (= (green fga) (green fgb))
                     (= (blue fga) (blue fgb))))
      (setf (getf differences :fg) fgb))
    (unless (or (and (null bga) (null bgb))
                (and bga bgb
                     (= (red bga) (red bgb))
                     (= (green bga) (green bgb))
                     (= (blue bga) (blue bgb))))
      (setf (getf differences :bg) bgb))
    (unless (eq (boldp a) (boldp b))
      (setf (getf differences :bold) (boldp b)))
    (unless (eq (italicp a) (italicp b))
      (setf (getf differences :italic) (italicp b)))
    (unless (eq (reversep a) (reversep b))
      (setf (getf differences :reverse) (reversep b)))
    (unless (eq (underlinep a) (underlinep b))
      (setf (getf differences :underline) (underlinep b)))
    differences))

;; TODO expose through customization interface
(defvar *default-bg-color* #x002B36)
(defvar *default-fg-color* #x839496)

(defvar *default-style* (make-style))

;;; syntax -> style mapping

(defvar *syntax->styles* (make-hash-table))

(defun syntax-style (syntax)
  (gethash syntax *syntax->styles*))

(defun (setf syntax-style) (new-value syntax)
  (setf (gethash syntax *syntax->styles*) new-value))

(defparameter *default-style*
  (make-style :fg *default-fg-color* :bg *default-bg-color*))
(setf (syntax-style :text) *default-style*)

;;; lexers TODO just use tree sitter

;; common lisp

(defparameter *cl-comment-regex*
  (ppcre:create-scanner "^;.+$|(?<=[^\\\\]);.*$" :single-line-mode t))

(defparameter *cl-global-regex*
  (ppcre:create-scanner "(?<=['(\\s:])\\*\\S*?\\*(?=[\\s)]|$)"))

(defparameter *cl-constant-regex* (ppcre:create-scanner "(?<=['(\\s:])(\\+\\S*?\\+|array-dimension-limit|array-rank-limit|array-total-size-limit|boole-1|boole-2|boole-andc1|boole-andc2|boole-and|boole-c1|boole-c2|boole-clr|boole-eqv|boole-ior|boole-nand|boole-nor|boole-orc1|boole-orc2|boole-set|boole-xor|call-arguments-limit|char-code-limit|double-float-epsilon|double-float-negative-epsilon|internal-time-units-per-second|lambda-list-keywords|lambda-parameters-limit|least-negative-double-float|least-negative-long-float|least-negative-normalized-double-float|least-negative-normalized-long-float|least-negative-normalized-short-float|least-negative-normalized-single-float|least-negative-short-float|least-negative-single-float|least-positive-double-float|least-positive-long-float|least-positive-normalized-double-float|least-positive-normalized-long-float|least-positive-normalized-short-float|least-positive-normalized-single-float|least-positive-short-float|least-positive-single-float|long-float-epsilon|long-float-negative-epsilon|most-negative-double-float|most-negative-fixnum|most-negative-long-float|most-negative-short-float|most-negative-single-float|most-positive-double-float|most-positive-fixnum|most-positive-long-float|most-positive-short-float|most-positive-single-float|multiple-values-limit|pi|short-float-epsilon|short-float-negative-epsilon|single-float-epsilon|single-float-negative-epsilon)(?=[\\s)]|$)"))

(defparameter *cl-read-macro-constant-regex*
  (ppcre:create-scanner "#(\\\\([()]|[^()\\s]+)|[box]\\d+|\\d+r\\d+|\\d?\\*[01]*)"
                        :single-line-mode t))

(defparameter *cl-special-constant-regex*
  (ppcre:create-scanner "(?<=['(\\s])([:&]\\S+?|t|nil|otherwise)(?=[\\s)]|$)"))

(defparameter *cl-non-function-regex* (ppcre:create-scanner "(?<=['(])(with-\\S*?|\\S*let\\*?|lambda|or|setf|assert|call-method|case|ccase|check-type|cond|ctypecase|decf|declaim|defclass|defconstant|defgeneric|define-compiler-macro|define-condition|define-method-combination|define-modify-macro|define-setf-expander|define-symbol-macro|defmacro|defmethod|defpackage|defparameter|defsetf|defstruct|deftype|defun|defvar|destructuring-bind|do|do-all-symbols|do-external-symbols|do-symbols|do\\*|dolist|dotimes|ecase|etypecase|formatter|handler-bind|handler-case|ignore-errors|in-package|incf|loop|loop-finish|make-method|multiple-value-bind|multiple-value-list|multiple-value-setq|nth-value|pop|pprint-exit-if-list-exhausted|pprint-logical-block|pprint-pop|print-unreadable-object|prog|prog1|prog2|prog\\*|psetf|psetq|push|pushnew|remf|restart-bind|restart-case|return|rotatef|shiftf|step|time|trace|typecase|unless|untrace|when|function|block|catch|eval-when|go|if|labels|load-time-value|locally|multiple-value-call|multiple-value-prog1|progn|progv|quote|return-from|setq|tagbody|the|throw|unwind-protect)(?=[\\s)]|$)"))

(defun cl-lexer (results line)
  (ppcre:do-scans (start end rs re *cl-global-regex* line)
    (fill results :slight-emphasis :start start :end end))
  (ppcre:do-scans (start end rs re *cl-non-function-regex* line)
    (fill results :obvious3 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-read-macro-constant-regex* line)
    (fill results :obvious2 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-constant-regex* line)
    (fill results :obvious1 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-special-constant-regex* line)
    (fill results :obvious0 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-comment-regex* line)
    (fill results :comment :start start :end end)))

;; todo

(let ((todo-regex (ppcre:create-scanner "todo|fixme|xxx" :case-insensitive-mode t)))
  (defun todo-lexer (results line)
    (ppcre:do-scans (start end rs re todo-regex line)
      (fill results :important :start start :end end))))
