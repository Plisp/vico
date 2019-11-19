(in-package :vico-term.util)

(ctype c-wchar-t "wchar_t")

#+unix
(progn

  (include "termios.h")
  (include "sys/ioctl.h")
  (ctype c-tcflag-t "tcflag_t")
  ;; struct winsize
  (cstruct c-winsize "struct winsize"
           (c-ws-rows "ws_row" :type :unsigned-short)
           (c-ws-cols "ws_col" :type :unsigned-short))
  (constant (c-get-winsz "TIOCGWINSZ"))
  (constant (c-set-winsz "TIOCSWINSZ"))
  ;; struct termios
  (cstruct c-termios "struct termios"
           (c-iflag "c_iflag" :type c-tcflag-t)
           (c-oflag "c_oflag" :type c-tcflag-t)
           (c-cflag "c_cflag" :type c-tcflag-t)
           (c-lflag "c_lflag" :type c-tcflag-t))
  (constant (c-set-attributes-now "TCSANOW"))
  (constant (c-icrnl "ICRNL"))
  (constant (c-ixon "IXON"))
  (constant (c-opost "OPOST"))
  (constant (c-icanon "ICANON"))
  (constant (c-isig "ISIG"))
  (constant (c-echo "ECHO"))
  (constant (c-echoe "ECHOE"))
  (constant (c-echok "ECHOK"))
  (constant (c-echonl "ECHONL")))
