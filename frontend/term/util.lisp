(in-package :vico-term.util)

(defun wide-character-width (character)
  (cffi:foreign-funcall wcwidth c-wchar-t (char-code character) :int))

(cffi:defcfun ioctl :int
  (fd :int)
  (cmd :int)
  &rest)

(defun get-term-dimensions (fd)
  "Returns a cons (LINES . COLUMNS) containing the dimensions of the terminal
device backing FD. Returns NIL on failure."
  (cffi:with-foreign-object (ws '(:struct c-winsize))
    (when (= 0 (ioctl fd c-get-winsz :pointer ws))
      (cffi:with-foreign-slots ((c-ws-rows c-ws-cols) ws (:struct c-winsize))
        (cons c-ws-rows c-ws-cols)))))

(defun set-term-dimensions (fd lines columns)
  "Sets the dimensions of the terminal device backing FD to LINESxCOLUMNS.
Returns T on success."
  (cffi:with-foreign-object (ws '(:struct c-winsize))
    (cffi:with-foreign-slots ((c-ws-rows c-ws-cols) ws (:struct c-winsize))
      (setf c-ws-rows lines c-ws-cols columns)
      (when (= 0 (ioctl fd c-set-winsz :pointer ws))
        t))))

(cffi:defcfun "tcgetattr" :int
  (fd :int)
  (termios-p (:pointer (:struct c-termios))))

(cffi:defcfun "tcsetattr" :int
  (fd :int)
  (optional-actions :int)
  (termios-p (:pointer (:struct c-termios))))

(defun setup-terminal-input ()
  "Disables terminal echoing and buffering. Returns a pointer to the original termios."
  (let ((orig-termios (cffi:foreign-alloc '(:struct c-termios))))
    (tcgetattr 0 orig-termios)
    (cffi:with-foreign-object (new-termios '(:struct c-termios))
      (tcgetattr 0 new-termios)
      (cffi:with-foreign-slots ((c-iflag c-oflag c-lflag) new-termios (:struct c-termios))
        (setf c-iflag (logandc2 c-iflag (logior c-icrnl c-ixon)))
        (setf c-oflag (logandc2 c-oflag c-opost))
        (setf c-lflag (logandc2 c-lflag (logior c-echo c-echoe c-echok c-icanon c-isig)))
        (tcsetattr 0 c-set-attributes-now new-termios) ;TODO error handling
        orig-termios))))

(defun restore-terminal-input (orig-termios)
  "Restores the terminal device backing FD to its original state. ORIG-TERMIOS is
a pointer to the original termios struct returned by a call to SETUP-TERM which
is freed. It will be set to NIL on success."
  (tcsetattr 0 c-set-attributes-now orig-termios)
  (cffi:foreign-free orig-termios)
  (setf orig-termios nil))
