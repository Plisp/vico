(defpackage :vico-core.syntax-highlighting
  (:use :cl)
  (:export #:color
           #:r #:g #:b

           #:style
           #:fg #:bg
           #:style-difference
           #:syntax-style
           #:*default-style*

           #:cl-lexer #:todo-lexer))
(in-package :vico-core.syntax-highlighting)

;;; syntax -> style

(defstruct (color (:conc-name nil))
  (r (error "must provide red value")   :type fixnum :read-only t)
  (g (error "must provide green value") :type fixnum :read-only t)
  (b (error "must provide blue value")  :type fixnum :read-only t))

(defclass style ()
  ((foreground :initarg :foreground
               :initform *default-fg-color*
               :reader fg
               :type color)
   (background :initarg :background
               :initform *default-bg-color*
               :reader bg
               :type color)
   (bold :initarg :bold
         :initform nil
         :reader bold)
   (italic :initarg :italic
           :initform nil
           :reader italic)
   (underline :initarg :underline
              :initform nil
              :reader underline))
  (:documentation "immutable"))

(defvar *syntax->styles* (make-hash-table))

(defun syntax-style (syntax)
  (gethash syntax *syntax->styles*))

(defun (setf syntax-style) (new-value syntax)
  (setf (gethash syntax *syntax->styles*) new-value))

(defun style-difference (a b)
  (let ((fga (fg a))
        (fgb (fg b))
        (bga (bg a))
        (bgb (bg b))
        differences)
    (unless (and (= (r fga) (r fgb))
                 (= (g fga) (g fgb))
                 (= (b fga) (b fgb)))
      (setf (getf differences :fg) fgb))
    (unless (and (= (r bga) (r bgb))
                 (= (g bga) (g bgb))
                 (= (b bga) (b bgb)))
      (setf (getf differences :bg) bgb))
    (unless (eq (bold a) (bold b))
      (setf (getf differences :bold) (bold b)))
    (unless (eq (italic a) (italic b))
      (setf (getf differences :italic) (italic b)))
    (unless (eq (underline a) (underline b))
      (setf (getf differences :underline) (underline b)))
    differences))

;; TODO expose through customization interface
(defvar *default-bg-color* (make-color :r 0 :g 43 :b 54))
(defvar *default-fg-color* (make-color :r 131 :g 148 :b 150))

(defvar *default-style*
  (make-instance 'style :foreground *default-fg-color* :background *default-bg-color*))
(setf (syntax-style :text) *default-style*)

;; default syntax elements - should be used for consistent colors
;; TODO color theme API

(defvar *default-comment-style*
  (make-instance 'style :foreground (make-color :r 88 :g 110 :b 117)))
(setf (syntax-style :comment) *default-comment-style*)

(defvar *slight-emphasis-style*
  (make-instance 'style :foreground (make-color :r 147 :g 161 :b 161) :italic t))
(setf (syntax-style :slight-emphasis) *slight-emphasis-style*)

(defvar *obvious3-style*
  (make-instance 'style :foreground (make-color :r 133 :g 153 :b 0)))
(setf (syntax-style :obvious3) *obvious3-style*)

(defvar *obvious2-style*
  (make-instance 'style :foreground (make-color :r 181 :g 137 :b 0)))
(setf (syntax-style :obvious2) *obvious2-style*)

(defvar *obvious1-style*
  (make-instance 'style :foreground (make-color :r 203 :g 75 :b 22)))
(setf (syntax-style :obvious1) *obvious1-style*)

(defvar *obvious0-style*
  (make-instance 'style :foreground (make-color :r 211 :g 54 :b 130)))
(setf (syntax-style :obvious0) *obvious0-style*)

(defvar *important-style*
  (make-instance 'style :foreground (make-color :r 220 :g 50 :b 47)))
(setf (syntax-style :important) *important-style*)

;;; lexers

;; TODO multiline comment/string highlighting

;; lex text into arrays of syntax classifications which are then mapped to display styles
;; this way different language highlighters can classify text consistently

;; lexers are straight lambdas for now

;; common lisp

(defparameter *cl-comment-regex*
  (ppcre:create-scanner "(?<=[^\\\\]);.*$" :single-line-mode t))

(defparameter *cl-global-regex*
  (ppcre:create-scanner "(?<=['(\\s:])\\*\\S*?\\*(?=[\\s)]|$)"))

(defparameter *cl-constant-regex* (ppcre:create-scanner "(?<=['(\\s:])(\\+\\S*?\\+|array-dimension-limit|array-rank-limit|array-total-size-limit|boole-1|boole-2|boole-andc1|boole-andc2|boole-and|boole-c1|boole-c2|boole-clr|boole-eqv|boole-ior|boole-nand|boole-nor|boole-orc1|boole-orc2|boole-set|boole-xor|call-arguments-limit|char-code-limit|double-float-epsilon|double-float-negative-epsilon|internal-time-units-per-second|lambda-list-keywords|lambda-parameters-limit|least-negative-double-float|least-negative-long-float|least-negative-normalized-double-float|least-negative-normalized-long-float|least-negative-normalized-short-float|least-negative-normalized-single-float|least-negative-short-float|least-negative-single-float|least-positive-double-float|least-positive-long-float|least-positive-normalized-double-float|least-positive-normalized-long-float|least-positive-normalized-short-float|least-positive-normalized-single-float|least-positive-short-float|least-positive-single-float|long-float-epsilon|long-float-negative-epsilon|most-negative-double-float|most-negative-fixnum|most-negative-long-float|most-negative-short-float|most-negative-single-float|most-positive-double-float|most-positive-fixnum|most-positive-long-float|most-positive-short-float|most-positive-single-float|multiple-values-limit|pi|short-float-epsilon|short-float-negative-epsilon|single-float-epsilon|single-float-negative-epsilon)(?=[\\s)]|$)"))

(defparameter *cl-read-macro-constant-regex*
  (ppcre:create-scanner "#(\\\\([()]|[^()\\s]+)|[box]\\d+|\\d+r\\d+|\\d?\\*[01]*)"
                        :single-line-mode t))

(defparameter *cl-special-constant-regex*
  (ppcre:create-scanner "(?<=['(\\s])([:&]\\S+?|t|nil|otherwise)(?=[\\s)]|$)"))

(defparameter *cl-non-function-regex* (ppcre:create-scanner "(?<=['(])(with-\\S*?|\\S*let\\*?|lambda|or|setf|assert|call-method|case|ccase|check-type|cond|ctypecase|decf|declaim|defclass|defconstant|defgeneric|define-compiler-macro|define-condition|define-method-combination|define-modify-macro|define-setf-expander|define-symbol-macro|defmacro|defmethod|defpackage|defparameter|defsetf|defstruct|deftype|defun|defvar|destructuring-bind|do|do-all-symbols|do-external-symbols|do-symbols|do\\*|dolist|dotimes|ecase|etypecase|formatter|handler-bind|handler-case|ignore-errors|in-package|incf|loop|loop-finish|make-method|multiple-value-bind|multiple-value-list|multiple-value-setq|nth-value|pop|pprint-exit-if-list-exhausted|pprint-logical-block|pprint-pop|print-unreadable-object|prog|prog1|prog2|prog\\*|psetf|psetq|push|pushnew|remf|restart-bind|restart-case|return|rotatef|shiftf|step|time|trace|typecase|unless|untrace|when|function|block|catch|eval-when|go|if|labels|load-time-value|locally|multiple-value-call|multiple-value-prog1|progn|progv|quote|return-from|setq|tagbody|the|throw|unwind-protect)(?=[\\s)]|$)"))

(defun cl-lexer (buffer results string start end) ;start,end in BUFFER not STRING
  (declare (ignore buffer start end))
  (ppcre:do-scans (start end rs re *cl-global-regex* string)
    (fill results :slight-emphasis :start start :end end))
  (ppcre:do-scans (start end rs re *cl-non-function-regex* string)
    (fill results :obvious3 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-read-macro-constant-regex* string)
    (fill results :obvious2 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-constant-regex* string)
    (fill results :obvious1 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-special-constant-regex* string)
    (fill results :obvious0 :start start :end end))
  (ppcre:do-scans (start end rs re *cl-comment-regex* string)
    (fill results :comment :start start :end end)))

;; todo

(let ((todo-regex (ppcre:create-scanner "todo|fixme|xxx" :case-insensitive-mode t)))
  (defun todo-lexer (buffer results string start end)
    (declare (ignore buffer start end))
    (ppcre:do-scans (start end rs re todo-regex string)
      (fill results :important :start start :end end))))
