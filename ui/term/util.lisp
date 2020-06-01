;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; split work into separate TUI library
;;
;; XXX on xterm eightBitInput should be disabled for meta keys (readme)
;; - with differing assumptions being made for different terminals

(in-package :vico-term.util)

(eval-when (:load-toplevel)
  (ffi:with-foreign-string (s "")
    (ffi:foreign-funcall "setlocale" :int c-lc-ctype :string s :pointer)))

;;; XXX this is broken, rewrite for CL-UNICODE
(defun character-width (character)
  "Returns the displayed width of CHARACTER and its string representation as multiple
values."
  (declare (optimize speed))
  (let ((codepoint (char-code character)))
    (cond ((= codepoint 0) (values 2 "^@")) ; NUL is ^@
          ((= codepoint 9) (values 8 "        ")) ; TODO tab character - should be variable
          ((<= #xD800 codepoint #xDFFF) (values 2 "�")) ; surrogate
          (t
           (let ((width (ffi:foreign-funcall "wcwidth" c-wchar codepoint :int)))
             (if (minusp width)
                 (if (< codepoint 32) ; caret notation
                     (values 2 (format nil "^~C" (code-char (logxor codepoint #x40))))
                     (values 2 "�"))
                 (values width (string character))))))))

(defun get-terminal-dimensions ()
  "Returns a cons (LINES . COLUMNS) containing the dimensions of the terminal device
backing FD. Returns NIL on failure."
  (ffi:with-foreign-object (ws '(:struct c-winsize))
    (when (= 0 (ffi:foreign-funcall "ioctl" :int 1 :int c-get-winsz :pointer ws :int))
      (ffi:with-foreign-slots ((c-ws-rows c-ws-cols) ws (:struct c-winsize))
        (return-from get-terminal-dimensions (cons c-ws-rows c-ws-cols)))))

  (let ((env-lines (uiop:getenv "LINES"))
        (env-columns (uiop:getenv "COLUMNS")))
    (when (and env-lines env-columns)
      (return-from get-terminal-dimensions (cons env-lines env-columns))))

  (cons 24 80))

(ffi:defcfun "tcgetattr" :int
  (fd :int)
  (termios-p (:pointer (:struct c-termios))))

(ffi:defcfun "tcsetattr" :int
  (fd :int)
  (optional-actions :int)
  (termios-p (:pointer (:struct c-termios))))

;;; input

(defun setup-terminal-input ()
  "Disables terminal echoing and buffering. Returns a pointer to the original termios."
  (format t "~c[?1006h~c[?1002h" #\esc #\esc)
  (let ((old-termios (ffi:foreign-alloc '(:struct c-termios))))
    (when (minusp (tcgetattr 0 old-termios))
      (error 'error:vico-syscall-error :format-control "tcgetattr failed"))
    (ffi:with-foreign-object (new-termios '(:struct c-termios))
      (setf (ffi:mem-ref new-termios '(:struct c-termios))
            (ffi:mem-ref old-termios '(:struct c-termios)))
      (ffi:with-foreign-slots ((c-iflag c-oflag c-lflag) new-termios (:struct c-termios))
        (setf c-iflag (logandc2 c-iflag (logior c-iexten c-inlcr c-istrip)))
        (setf c-iflag (logior c-iflag c-icrnl))
        (setf c-oflag (logandc2 c-oflag c-opost))
        (setf c-lflag (logandc2 c-lflag (logior c-icanon c-isig c-echo)))
        (when (minusp (tcsetattr 0 c-set-attributes-now new-termios))
          (error 'error:vico-syscall-error :format-control "tcsetattr failed"))
        old-termios))))

(defun restore-terminal-input (old-termios)
  "Restores the terminal device backing FD to its original state. ORIG-TERMIOS is a pointer
to the original termios struct returned by a call to SETUP-TERM which is freed. It will be
set to NIL on success."
  (format t "~c[?1002l~c[?1006l" #\esc #\esc)
  (when (minusp (tcsetattr 0 c-set-attributes-now old-termios))
    (error 'error:vico-syscall-error :format-control "tcsetattr failed"))
  (ffi:foreign-free old-termios)
  (values))

;; taken from acute-terminal-control READ-EVENT TODO rewrite

(symbol-macrolet ((read (read-char *standard-input* nil))) ;As we already know, xterm is very poorly and barely designed.
  (defun read-terminal-event (&optional (stream *standard-input*)
                              &aux (*standard-input* stream) (first read) second third)
    (block nil
      ;;Xterm likes to make its own standards for things, even when standards already exist.
      ;;This tested the eighth bit for the Meta key.
      ;;In a way, this arguably violated the character-set agnosticism, but not truly, I suppose.
      ;;In any case, I'll instead implement the escape-prefixing nonsense.  It couldn't be simple, no.
      ;;(if (logbitp 7 (char-code first))
      ;;    (return (cons :meta (code-char (ldb (byte 7 0) (char-code first))))))
      ;;This permits Escape being its own key without first needing more input.
      ;;Of course, this violates the principle that input should be waited on, but it seems no one cares about that.
      ;;So, you must send at least the first three characters of a control sequence at once for it to be recognized.
      (or (listen) (return first))
      (setq second read)
      ;;This implements the silly Meta key convention that prefixes with escape.
      ;;I must perform a trick to see if it's part of a control function or not.
      (if (eql #.(code-char #x1B) first)
          (if (or (eql #.(code-char #x5B) second)
                  (eql #.(code-char #x4F) second))
              (or (listen) (return (cons :meta second)))
              (return (cons :meta second))))
      (and (eql #.(code-char #x1B) first) ;;Xterm then likes to implement real standards incorrectly.
           (eql #.(code-char #x4F) second) ;;Why use FUNCTION KEY when you can use SS3?
           (setq third read)
           ;;Here's the pattern xterm uses for function keys one through twelve:
           ;;SS3 P     SS3 Q     SS3 R     SS3 S
           ;;CSI 1 5 ~ CSI 1 7 ~ CSI 1 8 ~ CSI 1 9 ~
           ;;CSI 2 0 ~ CSI 2 1 ~ CSI 2 3 ~ CSI 2 4 ~
           ;;Simplicity at its finest, you see.
           ;;With any luck, this garbage can be removed soon.
           (return (and (<= #x50 (char-code third) #x53)
                        (cons :function (- (char-code third) #x4F)))))
      (and (eql #.(code-char #x1B) first)
           (eql #.(code-char #x5B) second)
           (setq third read)
           (return (cond ((char= third #.(code-char #x41)) :up)
                         ((char= third #.(code-char #x42)) :down)
                         ((char= third #.(code-char #x43)) :right)
                         ((char= third #.(code-char #x44)) :left)
                         ((char= third #.(code-char #x4D)) ;xterm X10 mouse reporting
                          (let ((first (- (char-code (or read (return))) 32))
                                (second (- (char-code (or read (return))) 32)))
                            (ignore-errors ;whoever designed this protocol should be executed ;) agreed
                             (list* :mouse
                                    (mod (1+ (char-code (or read (return)))) 4)
                                    (if (> first 223) (return) first)
                                    (if (> second 223) (return) second)))))
                         ((char= third #.(code-char #x3C)) ;xterm SGR mouse reporting
                          (let* ((first (loop for c = read do (or c (return))
                                              until (char= c #.(code-char #x3B))
                                              do (if (or (char= c #.(code-char #x4D)) (char= c #.(code-char #x6D)))
                                                     (progn (unread-char c) (loop-finish)))
                                              do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return))
                                              collect c))
                                 (second (loop for c = read do (or c (return))
                                               until (char= c #.(code-char #x3B))
                                               do (if (or (char= c #.(code-char #x4D)) (char= c #.(code-char #x6D)))
                                                      (progn (unread-char c) (loop-finish)))
                                               do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return))
                                               collect c))
                                 (release)
                                 (third (loop named loop
                                              for c = read do (or c (return-from loop))
                                              until (char= c #.(code-char #x3B))
                                              do (if (char= c #.(code-char #x4D)) (loop-finish))
                                              do (when (char= c #.(code-char #x6D))
                                                   (setq release :release)
                                                   (loop-finish)) ;ignore mouse releases
                                              do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return-from loop))
                                              collect c))
                                 (first-default #.(make-string 1 :initial-element #.(code-char #x30)))
                                 (default #.(make-string 1 :initial-element #.(code-char #x31))))
                            (setq first (or (and first (make-array (length first) :element-type 'character :initial-contents first)) first-default)
                                  first (parse-integer first)
                                  second (or (and second (make-array (length second) :element-type 'character :initial-contents second)) default)
                                  third (or (and third (make-array (length third) :element-type 'character :initial-contents third)) default))
                            (if (= first 64) (return :scroll-up))
                            (if (= first 65) (return :scroll-down))
                            (if (= first 66) (return :scroll-left))
                            (if (= first 67) (return :scroll-right))
                            (list* (or release
                                       (case first (0 :mouse) (1 :mouse) (2 :mouse)
                                             (32 :drag) (34 :drag) (35 :hover) (51 :hover)))
                                   (ldb (byte 2 0) (1+ (if (> first 32) (- first 32) first)))
                                   (parse-integer second)
                                   (parse-integer third)
                                   (unless (zerop (ldb (byte 1 4) first)) :control))))
                         ;;Now, if the sequences sent were sane, I wouldn't even need this COND.
                         ;;Xterm sends CONTROL SEQUENCE INTRODUCER, but with characters other than parameters immediately following it.
                         ;;Instead, I need a different case for each and then I can do the parsing, which is still followed by other identifying characters.
                         ;;So, this could've been a beautiful function, I think, but the horrible design of xterm prevented that.
                         (t (let ((integer (parse-integer
                                            (coerce (cons third
                                                          (loop for char = read do (or char (return))
                                                                do (if (or (char= char #.(code-char #x20)) (char= char #.(code-char #x7E)))
                                                                       (progn (unread-char char) (loop-finish)))
                                                                do (or (char= char #.(code-char #x00)) (<= #x30 (char-code char) #x39) (return))
                                                                if (char/= char #.(code-char #x00)) collect char))
                                                    'string))))
                              (if (char= read #.(code-char #x7E)) ;This is the stupid convention that doesn't scale to infinity.

                                  (if-let ((function-key (case integer (11 1) (12 2) (13 3) (14 4) (15 5) (17 6) (18 7) (19 8) (20 9) (21 10)
                                                               (23 11) (24 12) (25 13) (26 14) (28 15) (29 16) (31 17) (32 18) (33 19) (34 20))))
                                    (cons :function function-key)
                                    ;; ugh
                                    (case integer (5 :page-up) (6 :page-down)))
                                  (if (and (char= read #.(code-char #x20)) ;This is FUNCTION KEY, the proper way to do this.
                                           (char= read #.(code-char #x57))) ;Unfortunately, I must do this in an ugly way.
                                      (cons :function integer))))))))
      (and second (not third) (unread-char second))
      first)))
