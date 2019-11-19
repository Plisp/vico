;ACUTE-TERMINAL-CONTROL - Provide for fast control of terminal devices.
;Copyright (C) 2018,2019 Prince Trippy programmer@verisimilitudes.net .

;This program is free software: you can redistribute it and/or modify it under the terms of the
;GNU Affero General Public License version 3 as published by the Free Software Foundation

;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;See the GNU Affero General Public License for more details.

;You should have received a copy of the GNU Affero General Public License along with this program.
;If not, see <http://www.gnu.org/licenses/>.

(cl:defpackage #:acute-terminal-control
  (:documentation "This package exports miscellaneous functions for control of terminal devices.")
  (:use #:common-lisp #:cl-ecma-48)
  #-ascii (:shadow #:parse-integer)
  (:shadowing-import-from #:common-lisp #:null #:substitute #:ed)
  (:export #:cursor #:dimensions #:foreground #:background #:properties #:erase #:reset #:sound
           #:insert-mode #:replace-mode #:read-event #:read-event-no-hang #:scroll
           #:enable-system-echoing #:disable-system-echoing
           #:enable-system-buffering #:disable-system-buffering)
  (:nicknames #:atc))

(cl:in-package #:acute-terminal-control)

#-ascii
(defun parse-integer (string &aux (value 0))
  "Return an integer read from string using characters #x30 to #x39."
  (map nil (lambda (c) (setq value (+ c (* 10 value))))
       (map 'vector (lambda (c) (- (char-code c) #x30))
            string))
  value)

;(defmacro parse (reader ender &optional block)
;  ""
;  `(loop for c = ,reader do (or c (return-from ,block))
;         until (char= c #.(code-char #x3B))
;         do (if ,ender (progn (unread-char c) (loop-finish)))
;         do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return-from ,block))
;         collect c))

;(defun parse (end &optional last (test 'char=) (stream *standard-input*) &aux (*standard-input* stream))
;  ""
;  (prog (c l e) :begin
;        (or (setq c (read-char-no-hang *standard-input* nil)) (return))
;        (if (char= c #.(code-char #x3B))
;            (if last (return) (go :end)))
;        (if (funcall test c end) (go :end))
;        (or (<= #x30 (char-code c) #x39) (return))
;        (if l (setq e (cdr (rplacd e (cons c nil))))
;            (setq l (cons c nil) e l))
;        :end (make-array (length l) :element-type 'character :initial-contents l)))

(defun cursor (&optional (stream *standard-output*)
               &aux (*standard-input* (if (eq stream *standard-output*) *standard-input* stream))
                    (*standard-output* stream) car cdr)
  "Return a CONS containing the line and character position of the cursor in STREAM.
If this can't be determined, return NIL.
If the stream is *STANDARD-OUTPUT*, *STANDARD-INPUT* is implicitly used for input.
If not, the stream should be a TWO-WAY-STREAM.
Calls CLEAR-INPUT and FINISH-OUTPUT."
  (finish-output)
  (clear-input)
  (device-status-report 6)
  (finish-output)
  (or (interactive-stream-p *standard-input*) (listen) (return-from cursor))
  (or (listen) (sleep 0.01) (listen) (sleep 0.03) (listen) (sleep 0.06) (listen) (sleep 0.08)
      (listen) (sleep 0.12) (listen) (sleep 0.15) (listen) (return-from cursor))
  (prog (char)
   :start1 (or (setq char (read-char-no-hang *standard-input* nil)) (go :end))
   (if (char= char #.(code-char #x00)) (go :start1))
   (or (char= char #.(code-char #x1B)) (go :end))
   :start2 (or (setq char (read-char-no-hang *standard-input* nil)) (go :end))
   (if (char= char #.(code-char #x00)) (go :start2))
   (or (char= char #.(code-char #x5B)) (go :end))
   (return)
   :end (return-from cursor))
  (setq car (loop for char = (read-char-no-hang *standard-input* nil)
                  unless char do (return-from cursor)
                  until (char= char #.(code-char #x3B))
                  do (if (char= char #.(code-char #x52)) (progn (unread-char char) (loop-finish)))
                  do (or (char= char #.(code-char #x00)) (<= #x30 (char-code char) #x39) (return-from cursor))
                  if (char/= char #.(code-char #x00)) collect char)
        car (or car #.(make-string 1 :initial-element (code-char #x31)))
        cdr (loop for char = (read-char-no-hang *standard-input* nil)
                  unless char do (return-from cursor)
                  until (char= char #.(code-char #x52))
                  do (or (char= char #.(code-char #x00)) (<= #x30 (char-code char) #x39) (return-from cursor))
                  if (char/= char #.(code-char #x00)) collect char)
        cdr (or cdr #.(make-string 1 :initial-element (code-char #x31))))
  (cons (parse-integer (coerce car 'string))
        (parse-integer (coerce cdr 'string))))

(defun (setf cursor) (cons &optional (stream *standard-output*)
                      &aux (*standard-output* stream))
  "Reposition the cursor based on the value (line . character); (NIL) and NIL are equivalent to (1 . 1)."
  (cursor-position (car cons) (cdr cons))
  cons)

(defun dimensions (&optional (stream *standard-output*) &aux (*standard-output* stream))
  "Return a CONS containing the line and character limits; calls CURSOR.
If this can't be determined, return NIL."
  (let (position (last (cursor)))
    (if last
        (unwind-protect (setf (cursor) '(999999 . 999999)
                              position (cursor))
          (setf (cursor) last)
          position))))

;For ISO 8613-6 color, we send a sequence SGR 38 followed by other values.
;For RGB, the entire sequence is SGR 38 2 NIL R G B where the last three parameters are integers.
;Previously, there was a different SGR being used that didn't optimize away zeroes.
;However, as zero is the default for all SGR parameters, this is no longer done and simplifies it.

(let ((colors #(:black :red :green :yellow :blue :magenta :cyan :white :extended :default))
      (qualities '(:default 0 :bold 1 :faint 2 :italics 3 :underlined 4 :blinking 5 ;:slow-blinking 5 :fast-blinking 6
                   :negative 7 :concealed 8 :crossed-out 9 :positive 27 :revealed 28)))
  (defun (setf foreground) (color &optional (stream *standard-output*)
                            &aux (*standard-output* stream))
    "Set the color of the foreground in STREAM named by a keyword."
    #1=(progn (or (and (listp color)
                       (member (car color) '(:rgb :cmy :cmyk))
                       (every 'integerp (cdr color))
                       (= (length color) (if (eq :cmyk (car color)) 5 4)))
                  (eq color :transparent)
                  (position color colors :test 'eq)
                  (error "Invalid color: ~a" color)))
    (sgr (cond ((listp color)
                ;The NIL should specify the default color space identifier specified in ISO 8613-6.
                ;Unfortunately, giving a default value for this isn't parsed correctly by any terminal emulators, it seems.
                ;So, it is omitted for now.
                ;It will otherwise shift the colors under many implementations.
                ;You'd expect something as simple as this to be done correctly, but it's not.
                ;See page 49 of the standard; if 2, 3, or 4, there's a second parameter followed by more.
                ;That second parameter is the color space identifier, followed by the actual color integers.
                ;This second parameter is erroneously ignored, however, in a way that probably won't ever be corrected.
                ;I'll be submitting bug reports to the terminal emulators.
                ;So, perhaps I'll be able to send the correct sequence in a year or two.

                ;The same page specifies that character #x3A, colon, may be used to separate these parameters.
                ;Since that's a ``may'', the semicolon is used; at least this simplifies the program.
                ;Some terminal emulators don't parse the colons correctly anyway, but I don't care about that.

                ;If you'd like to learn more about the blatant violation of standards, read The UNIX-HATERS Handbook.
                (list* 38 (case (car color) (:rgb 2) (:cmy 3) (:cmyk 4))
                       #-(or unix posix) nil (cdr color)))
               ((eq color :transparent) '(38 1))
               (t (+ 30 (position color colors :test 'eq)))))
    color)

  (defun (setf background) (color &optional (stream *standard-output*)
                            &aux (*standard-output* stream))
    "Set the color of the background in STREAM named by a keyword."
    #1#
    (sgr (cond ((listp color)
                ;See the above, most recent comment.
                (list* 48 (case (car color) (:rgb 2) (:cmy 3) (:cmyk 4))
                       #-(or unix posix) nil (cdr color)))
               ((eq color :transparent) '(48 1))
               (t (+ 40 (position color colors :test 'eq)))))
    color)

  (defun (setf properties) (property &optional (stream *standard-output*)
                            &aux (*standard-output* stream))
    "Set the properties, sans colors, of the text in STREAM named by a keyword.
If :DEFAULT is used, all properties, including colors, will be set to the default."
    (or (member property qualities :test 'eq)
        (error "Invalid property: ~a" property))
    (sgr (getf qualities property 0))
    property))

(defun erase (&optional (stream *standard-output*)
              &aux (*standard-output* stream))
  "Erase the contents of STREAM."
  (erase-in-page 3) ;This is followed by a standard sequence in case it's unsupported.
  (erase-in-page 2))

(defun scroll (keyword &optional (count 1) (stream *standard-output*) &aux (*standard-output* stream))
  "Scroll STREAM :UP, :DOWN, :LEFT, or :RIGHT by COUNT."
  (ecase keyword
    (:up (scroll-up count))
    (:down (scroll-down count))
    (:left (scroll-left count))
    (:right (scroll-right count))))

(setf (symbol-function 'reset) (symbol-function 'reset-to-initial-state)
      (documentation 'reset 'function) "Set STREAM to its default state."
      (symbol-function 'sound) (symbol-function 'bell)
      (documentation 'sound 'function) "Alert STREAM, typically with a flash or sound."
      (symbol-function 'insert-mode) (lambda (&optional (stream *standard-output*) &aux (*standard-output* stream)) (sm 4))
      (documentation 'insert-mode 'function) "Have new input to STREAM insert itself."
      (symbol-function 'replace-mode) (lambda (&optional (stream *standard-output*) &aux (*standard-output* stream)) (rm 4))
      (documentation 'replace-mode 'function) "Have new input to STREAM replace or merge with the current state."
      (symbol-function 'read-event)
      (symbol-macrolet ((read (read-char *standard-input* nil))) ;As we already know, xterm is very poorly and barely designed.
        #1=(lambda (&optional (stream *standard-input*)
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
                                         (third (loop named loop for c = read do (or c (return-from loop))
                                                      until (char= c #.(code-char #x3B))
                                                      do (if (char= c #.(code-char #x4D)) (loop-finish))
                                                      do (if (char= c #.(code-char #x6D)) (return nil)) ;ignore mouse releases
                                                      do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return-from loop))
                                                      collect c))
                                         (first-default #.(make-string 1 :initial-element #.(code-char #x30)))
                                         (default #.(make-string 1 :initial-element #.(code-char #x31))))
                                     (setq first (or (make-array (length first) :element-type 'character :initial-contents first) first-default)
                                           first (parse-integer first)
                                           second (or (make-array (length second) :element-type 'character :initial-contents second) default)
                                           third (or (make-array (length third) :element-type 'character :initial-contents third) default))
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
      (documentation 'read-event 'function)
      "Return a higher level event, blocking, or NIL if one can't be determined.
A character is returned as with READ-CHAR if that is the highest event.
A keyword denoting a special key is returned in the case of higher level key presses.
A CONS containing a KEYWORD and a value is returned in the case of some higher level key presses.
A list of the form (:MOUSE Z X Y . W) is returned in the case of a mouse event.
A CONS of the form (:FUNCTION . N) is returned in the case of a ``function key'' event.
A CONS of the form (:META . C) is returned in the case of a meta key event.
The KEYWORD :UP, :DOWN, :LEFT, or :RIGHT is returned in the case of an ``arrow key'' event."
      (symbol-function 'read-event-no-hang) (symbol-macrolet ((read (read-char-no-hang *standard-input* nil))) #1#)
      (documentation 'read-event-no-hang 'function)
      "Return a higher level event or NIL if one can't be determined or isn't immediately available.
A character is returned as with READ-CHAR-NO-HANG if that is the highest event.
A keyword denoting a special key is returned in the case of higher level key presses.
A CONS containing a KEYWORD and a value is returned in the case of some higher level key presses.
A list of the form (:MOUSE Z X Y . W) is returned in the case of a mouse event.
A CONS of the form (:FUNCTION . N) is returned in the case of a ``function key'' event.
A CONS of the form (:META . C) is returned in the case of a meta key event.
The KEYWORD :UP, :DOWN, :LEFT, or :RIGHT is returned in the case of an ``arrow key'' event.")

(warn "The functions ENABLE-SYSTEM-BUFFERING, DISABLE-SYSTEM-BUFFERING, ENABLE-SYSTEM-ECHOING, ~
and DISABLE-SYSTEM-ECHOING, cannot be implemented in portable Common Lisp.
They are married to the semantics of poor operating systems.
Why have READ-CHAR and READ-LINE and others, if they're broken by buffering anyway?
Why be unable to use the ECHO-STREAM for input echoing if the system still insists on this little game?
Strongly consider using a wrapper program around the Common Lisp implementation, instead.")

;#+ecl
;(eval-when (:compile-toplevel)
;  (ffi:clines "#include <termios.h>"))

#+ecl
(ignore-errors (ffi:clines "#include <termios.h>"))

#+ecl
(eval-when (:load-toplevel :execute)
  (warn "ECL requires ACUTE-TERMINAL-CONTROL to be compiled for these four functions to work properly."))

(defun enable-system-buffering (&optional (stream *standard-input*))
  "Tell the system to break READ-CHAR and others.
This enables the standard system buffering available to the best of its ability."
  #-(or allegro ccl mcl openmcl)
  ;Many implementations won't get ``file descriptors'' for these types of streams.
  (ignore-errors (tagbody :begin
                    (and (typep stream 'synonym-stream)
                         (setq stream (symbol-value (synonym-stream-symbol stream)))
                         (go :begin))
                    (and (typep stream 'two-way-stream)
                         (setq stream (two-way-stream-input-stream stream))
                         (go :begin))))
  (ignore-errors #+abcl ()
                 #+allegro (let (io (excl.osi:tcgetattr stream))
                             (setf (excl.osi:termios-oflag io) (logior (excl.osi:termios-oflag io) excl.osi:*opost*)
                                   (excl.osi:termios-iflag io) (logior (excl.osi:termios-iflag io) excl.osi:*icrnl* excl.osi:*ixon*)
                                   (excl.osi:termios-lflag io) (logior (excl.osi:termios-lflag io) excl.osi:*icanon* excl.osi:*isig*))
                             (excl.osi:tcsetattr stream io))
                 #+(or ccl mcl openmcl) ()
                 #+clasp ()
                 #+(or clisp ufasoft-lisp) () ;ext:*keyboard-input*
                 #+cmu (let ((file-stream (system:fd-stream-fd stream)))
                         (alien:with-alien ((io (alien:struct unix:termios)))
                           (unix:unix-tcgetattr file-stream io)
                           (setf (alien:slot io 'unix:c-oflag) (logior (alien:slot io 'unix:c-oflag) unix:tty-opost)
                                 (alien:slot io 'unix:c-iflag) (logior (alien:slot io 'unix:c-iflag) unix:tty-icrnl unix:tty-ixon)
                                 (alien:slot io 'unix:c-lflag) (logior (alien:slot io 'unix:c-lflag) unix:tty-icanon unix:tty-isig))
                           (unix:unix-tcsetattr file-stream unix:tcsanow io)))
                 #+cormanlisp ()
                 #+ecl (ffi:c-inline ((ext:file-stream-fd stream)) (:int) (values)
                         "{struct termios t; tcgetattr(#0, &t); t.c_oflag |= OPOST; t.c_iflag |= ICRNL|IXON;
                           t.c_lflag |= ICANON|ISIG; tcsetattr(#0, TCSANOW, &t);}")
                 #+(or gcl wcl) ()
                 #+lispworks ()
                 #+mkcl ()
                 #+poplog () ;poplog:*raw-terminal-io*
                 #+sbcl (let* ((file-stream (sb-posix:file-descriptor stream))
                               (io (sb-posix:tcgetattr file-stream)))
                          (setf (sb-posix:termios-oflag io) (logior (sb-posix:termios-oflag io) sb-posix:opost)
                                (sb-posix:termios-iflag io) (logior (sb-posix:termios-iflag io) sb-posix:icrnl sb-posix:ixon)
                                (sb-posix:termios-lflag io) (logior (sb-posix:termios-lflag io) sb-posix:icanon sb-posix:isig))
                          (sb-posix:tcsetattr file-stream sb-posix:tcsanow io))
                 #+scl ()
                 #+xcl ())
  (values))

;This garbage is potentially relevant later:
;ignbrk brkint ignpar parmrk inpck istrip inlcr igncr ixoff

(defun disable-system-buffering (&optional (stream *standard-input*))
  "Tell the system to break READ-LINE and others.
This disables the standard system buffering available to the best of its ability."
  #-(or allegro ccl mcl openmcl)
  ;Many implementations won't get ``file descriptors'' for these types of streams.
  (ignore-errors (tagbody :begin
                    (and (typep stream 'synonym-stream)
                         (setq stream (symbol-value (synonym-stream-symbol stream)))
                         (go :begin))
                    (and (typep stream 'two-way-stream)
                         (setq stream (two-way-stream-input-stream stream))
                         (go :begin))))
  (ignore-errors #+abcl ()
                 #+allegro (let (io (excl.osi:tcgetattr stream))
                             (setf (excl.osi:termios-oflag io) (logandc2 (excl.osi:termios-oflag io) excl.osi:*opost*)
                                   (excl.osi:termios-iflag io) (logandc2 (excl.osi:termios-iflag io) (logior excl.osi:*icrnl* excl.osi:*ixon*))
                                   (excl.osi:termios-lflag io) (logandc2 (excl.osi:termios-lflag io) (logior excl.osi:*icanon* excl.osi:*isig*)))
                             (excl.osi:tcsetattr stream io))
                 #+(or ccl mcl openmcl) ()
                 #+clasp ()
                 #+(or clisp ufasoft-lisp) () ;ext:*keyboard-input*
                 #+cmu (let ((file-stream (system:fd-stream-fd stream)))
                         (alien:with-alien ((io (alien:struct unix:termios)))
                           (unix:unix-tcgetattr file-stream io)
                           (setf (alien:slot io 'unix:c-oflag) (logandc2 (alien:slot io 'unix:c-oflag) unix:tty-opost)
                                 (alien:slot io 'unix:c-iflag) (logandc2 (alien:slot io 'unix:c-iflag) (logior unix:tty-icrnl unix:tty-ixon))
                                 (alien:slot io 'unix:c-lflag) (logandc2 (alien:slot io 'unix:c-lflag) (logior unix:tty-icanon unix:tty-isig)))
                           (unix:unix-tcsetattr file-stream unix:tcsanow io)))
                 #+cormanlisp ()
                 #+ecl (ffi:c-inline ((ext:file-stream-fd stream)) (:int) (values)
                         "{struct termios t; tcgetattr(#0, &t); t.c_oflag &= ~OPOST; t.c_iflag &= ~ICRNL|~IXON;
                           t.c_lflag &= ~ICANON|~ISIG; tcsetattr(#0, TCSANOW, &t);}")
                 #+(or gcl wcl) ()
                 #+lispworks ()
                 #+mkcl ()
                 #+poplog () ;poplog:*raw-terminal-io*
                 #+sbcl (let* ((file-stream (sb-posix:file-descriptor stream))
                               (io (sb-posix:tcgetattr file-stream)))
                          (setf (sb-posix:termios-oflag io) (logandc2 (sb-posix:termios-oflag io) sb-posix:opost)
                                (sb-posix:termios-iflag io) (logandc2 (sb-posix:termios-iflag io) (logior sb-posix:icrnl sb-posix:ixon))
                                (sb-posix:termios-lflag io) (logandc2 (sb-posix:termios-lflag io) (logior sb-posix:icanon sb-posix:isig)))
                          (sb-posix:tcsetattr file-stream sb-posix:tcsanow io))
                 #+scl ()
                 #+xcl ())
  (values))

(defun enable-system-echoing (&optional (stream *standard-input*))
  "Have the system show input as it's entered to the best of its ability."
  #-(or allegro ccl mcl openmcl)
  ;Many implementations won't get ``file descriptors'' for these types of streams.
  (ignore-errors (tagbody :begin
                    (and (typep stream 'synonym-stream)
                         (setq stream (symbol-value (synonym-stream-symbol stream)))
                         (go :begin))
                    (and (typep stream 'two-way-stream)
                         (setq stream (two-way-stream-input-stream stream))
                         (go :begin))))
  (ignore-errors #+abcl ()
                 #+allegro (excl.osi:enable-terminal-echo stream)
                 #+(or ccl mcl openmcl) (progn (require "PTY")
                                               (ccl::enable-tty-local-modes (or (ccl::stream-device stream :input)
                                                                                (ccl::stream-device stream :output))
                                        ;#$ECHO #$ECHOE #$ECHOK #$ECHONL
                                                                            ))
                 #+clasp ()
                 #+(or clisp ufasoft-lisp) ()
                 #+cmu (let ((file-stream (system:fd-stream-fd stream)))
                         (alien:with-alien ((io (alien:struct unix:termios)))
                           (unix:unix-tcgetattr file-stream io)
                           (setf (alien:slot io 'unix:c-lflag) (logior (alien:slot io 'unix:c-lflag)
                                                                       unix:tty-echo unix:tty-echoe
                                                                       unix:tty-echok unix:tty-echonl))
                           (unix:unix-tcsetattr file-stream unix:tcsanow io)))
                 #+cormanlisp ()
                 #+ecl (ffi:c-inline ((ext:file-stream-fd stream)) (:int) (values)
                         "{struct termios t; tcgetattr(#0, &t); t.c_lflag |= ECHO|ECHOE|ECHOK|ECHONL; tcsetattr(#0, TCSANOW, &t);}")
                 #+(or gcl wcl) ()
                 #+lispworks ()
                 #+mkcl ()
                 #+poplog ()
                 #+sbcl (let* ((file-stream (sb-posix:file-descriptor stream))
                               (io (sb-posix:tcgetattr file-stream)))
                          (setf (sb-posix:termios-lflag io)
                                (logior (sb-posix:termios-lflag io)
                                        sb-posix:echo sb-posix:echoe sb-posix:echok sb-posix:echonl))
                          (sb-posix:tcsetattr file-stream sb-posix:tcsanow io))
                 #+scl ()
                 #+xcl ())
  (values))

(defun disable-system-echoing (&optional (stream *standard-input*))
  "Have the system avoid showing input as it's entered to the best of its ability."
  #-(or allegro ccl mcl openmcl)
  ;Many implementations won't get ``file descriptors'' for these types of streams.
  (ignore-errors (tagbody :begin
                    (and (typep stream 'synonym-stream)
                         (setq stream (symbol-value (synonym-stream-symbol stream)))
                         (go :begin))
                    (and (typep stream 'two-way-stream)
                         (setq stream (two-way-stream-input-stream stream))
                         (go :begin))))
  (ignore-errors #+abcl ()
                 #+allegro (excl.osi:disable-terminal-echo stream)
                 #+(or ccl mcl openmcl) (progn (require "PTY")
                                               (ccl::disable-tty-local-modes (or (ccl::stream-device stream :input)
                                                                                 (ccl::stream-device stream :output))
                                        ;#$ECHO #$ECHOE #$ECHOK #$ECHONL
                                                                             ))
                 #+clasp ()
                 #+(or clisp ufasoft-lisp) ()
                 #+cmu (let ((file-stream (system:fd-stream-fd stream)))
                         (alien:with-alien ((io (alien:struct unix:termios)))
                           (unix:unix-tcgetattr file-stream io)
                           (setf (alien:slot io 'unix:c-lflag) (logandc2 (alien:slot io 'unix:c-lflag)
                                                                         (logior unix:tty-echo unix:tty-echoe
                                                                                 unix:tty-echok unix:tty-echonl)))
                           (unix:unix-tcsetattr file-stream unix:tcsanow io)))
                 #+cormanlisp ()
                 #+ecl (ffi:c-inline ((ext:file-stream-fd stream)) (:int) (values)
                         "{struct termios t; tcgetattr(#0, &t); t.c_lflag &= ~(ECHO|ECHOE|ECHOK|ECHONL); tcsetattr(#0, TCSANOW, &t);}")
                 #+(or gcl wcl) ()
                 #+lispworks ()
                 #+mkcl ()
                 #+poplog ()
                 #+sbcl (let* ((file-stream (sb-posix:file-descriptor stream))
                               (io (sb-posix:tcgetattr file-stream)))
                          (setf (sb-posix:termios-lflag io)
                                (logandc2 (sb-posix:termios-lflag io)
                                          (logior sb-posix:echo sb-posix:echoe sb-posix:echok sb-posix:echonl)))
                          (sb-posix:tcsetattr file-stream sb-posix:tcsanow io))
                 #+scl ()
                 #+xcl ())
  (values))
