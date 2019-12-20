(in-package :vico-term.util)

(defun wide-character-width (character)
  (cffi:foreign-funcall "wcwidth" c-wchar-t (char-code character) :int))

(cffi:defcfun ioctl :int
  (fd :int)
  (cmd :int)
  &rest)

(defun get-terminal-dimensions ()
  "Returns a cons (LINES . COLUMNS) containing the dimensions of the terminal device
backing FD. Returns NIL on failure."
  (cffi:with-foreign-object (ws '(:struct c-winsize))
    (when (= 0 (ioctl 1 c-get-winsz :pointer ws))
      (cffi:with-foreign-slots ((c-ws-rows c-ws-cols) ws (:struct c-winsize))
        (return-from get-terminal-dimensions (cons c-ws-rows c-ws-cols)))))

  (let ((env-lines (uiop:getenv "LINES"))
        (env-columns (uiop:getenv "COLUMNS")))
    (when (and env-lines env-columns)
      (return-from get-terminal-dimensions (cons env-lines env-columns))))

  (cons 24 80))

(cffi:defcfun "tcgetattr" :int
  (fd :int)
  (termios-p (:pointer (:struct c-termios))))

(cffi:defcfun "tcsetattr" :int
  (fd :int)
  (optional-actions :int)
  (termios-p (:pointer (:struct c-termios))))

;;; input

(defun setup-terminal-input ()
  "Disables terminal echoing and buffering. Returns a pointer to the original termios.
TODO just use stty w/ wrapper script."
  (let ((old-termios (cffi:foreign-alloc '(:struct c-termios))))
    (tcgetattr 0 old-termios)
    (cffi:with-foreign-object (new-termios '(:struct c-termios))
      (setf (cffi:mem-ref new-termios '(:struct c-termios))
            (cffi:mem-ref old-termios '(:struct c-termios)))
      (cffi:with-foreign-slots ((c-iflag c-oflag c-lflag) new-termios (:struct c-termios))
        (setf c-iflag (logandc2 c-iflag (logior c-icrnl c-inlcr c-istrip c-ixon)))
        (setf c-oflag (logandc2 c-oflag c-opost))
        (setf c-lflag (logandc2 c-lflag (logior c-icanon c-isig c-echo c-echoe c-echok c-echonl)))
        (tcsetattr 0 c-set-attributes-now new-termios) ;TODO error handling
        old-termios))))

(defun restore-terminal-input (old-termios)
  "Restores the terminal device backing FD to its original state. ORIG-TERMIOS is a pointer
to the original termios struct returned by a call to SETUP-TERM which is freed. It will be
set to NIL on success."
  (tcsetattr 0 c-set-attributes-now old-termios)
  (cffi:foreign-free old-termios)
  (setf old-termios nil))

;; taken from acute-terminal-control READ-EVENT TODO XXX replace with libtermkey rewrite
(symbol-macrolet ((read (read-char *standard-input* nil))) ;As we already know, xterm is very poorly and barely designed.
  (defun read-terminal-event (&optional (stream *standard-input*)
                              &aux (*standard-input* stream) (first read) second third)
    (block nil ;Xterm likes to make its own standards for things, even when standards already exist.
                                        ;This tested the eighth bit for the Meta key.
                                        ;In a way, this arguably violated the character-set agnosticism, but not truly, I suppose.
                                        ;In any case, I'll instead implement the escape-prefixing nonsense.  It couldn't be simple, no.
                                        ;(if (logbitp 7 (char-code first))
                                        ;    (return (cons :meta (code-char (ldb (byte 7 0) (char-code first))))))
                                        ;This permits Escape being its own key without first needing more input.
                                        ;Of course, this violates the principle that input should be waited on, but it seems no one cares about that.
                                        ;So, you must send at least the first three characters of a control sequence at once for it to be recognized.
      (or (listen) (return first))
      (setq second read)
                                        ;This implements the silly Meta key convention that prefixes with escape.
                                        ;I must perform a trick to see if it's part of a control function or not.
      (if (eql #.(code-char #x1B) first)
          (if (or (eql #.(code-char #x5B) second)
                  (eql #.(code-char #x4F) second))
              (or (listen) (return (cons :meta second)))
              (return (cons :meta second))))
                                        ;This implements part of what xterm uses for function keys.
      (and (eql #.(code-char #x1B) first) ;Xterm then likes to implement real standards incorrectly.
           (eql #.(code-char #x4F) second) ;Why use FUNCTION KEY when you can use SS3?
           (setq third read) ;Xterm is like a retarded child and should be put down.
                                        ;Here's the pattern xterm uses for function keys one through twelve:
                                        ;SS3 P     SS3 Q     SS3 R     SS3 S
                                        ;CSI 1 5 ~ CSI 1 7 ~ CSI 1 8 ~ CSI 1 9 ~
                                        ;CSI 2 0 ~ CSI 2 1 ~ CSI 2 3 ~ CSI 2 4 ~
                                        ;Simplicity at its finest, you see.
                                        ;With any luck, this garbage can be removed soon.
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
                            (ignore-errors ;whoever designed this protocol should be executed
                             (list* :mouse
                                    (mod (1+ (char-code (or read (return)))) 4)
                                    (if (> first 223) (return) first)
                                    (if (> second 223) (return) second)))))
                         ((char= third #.(code-char #x3C)) ;xterm SGR mouse reporting
                          (let ((first (loop for c = read do (or c (return))
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
                                (third (loop named loop
                                             for c = read do (or c (return-from loop))
                                             until (char= c #.(code-char #x3B))
                                             do (if (char= c #.(code-char #x4D)) (loop-finish))
                                             do (if (char= c #.(code-char #x6D)) (return)) ;ignore mouse releases
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
                            (list* (if (> first 32) :hover :mouse)
                                   (ldb (byte 2 0) (1+ (if (> first 32) (- first 32) first)))
                                   (parse-integer second)
                                   (parse-integer third)
                                   (prog (l)
                                      (or (zerop (ldb (byte 1 3) first)) (push :meta l))
                                      (or (zerop (ldb (byte 1 4) first)) (push :control l))
                                      (or (zerop (ldb (byte 1 2) first)) (push :shift l))
                                      (return l)))))
                                        ;Now, if the sequences sent were sane, I wouldn't even need this COND.
                                        ;Xterm sends CONTROL SEQUENCE INTRODUCER, but with characters other than parameters immediately following it.
                                        ;I can only figure this is to aid parsing by allowing one to quickly figure out what the control sequence (if you'd still call it that) is.
                                        ;While that does aid parsing if you just want a poor job, it actually complicates comprehensive parsing, like this.
                                        ;I can't just have a loop reading in parameters and then at the end matching a single character or two to see what it is.
                                        ;I can't just then check to see if the number of parameters read in is correct for the corresponding identity.
                                        ;I can't just have a limit to keep out of an infinite loop; three would be a good limit here, currently, since nothing supported needs more.
                                        ;Instead, I need a different case for each and then I can do the parsing, which is still followed by other identifying characters.
                                        ;So, this could've been a beautiful function, I think, but the horrible design of xterm prevented that.
                                        ;Instead, this is a purely practical machinery and I can only think about how nice it would've looked if things hadn't been designed by idiots.
                         (t (let ((integer (parse-integer
                                            (coerce (cons third
                                                          (loop for char = read do (or char (return))
                                                                do (if (or (char= char #.(code-char #x20)) (char= char #.(code-char #x7E)))
                                                                       (progn (unread-char char) (loop-finish)))
                                                                do (or (char= char #.(code-char #x00)) (<= #x30 (char-code char) #x39) (return))
                                                                if (char/= char #.(code-char #x00)) collect char))
                                                    'string))))
                              (if (char= read #.(code-char #x7E)) ;This is the stupid convention that doesn't scale to infinity.
                                  (cons :function (case integer (11 1) (12 2) (13 3) (14 4) (15 5) (17 6) (18 7) (19 8) (20 9) (21 10)
                                                        (23 11) (24 12) (25 13) (26 14) (28 15) (29 16) (31 17) (32 18) (33 19) (34 20)
                                                        (t (return))))
                                  (if (and (char= read #.(code-char #x20)) ;This is FUNCTION KEY, the proper way to do this.
                                           (char= read #.(code-char #x57))) ;Unfortunately, I must do this in an ugly way.
                                      (cons :function integer))))))))
      (and second (not third) (unread-char second))
      first)))
