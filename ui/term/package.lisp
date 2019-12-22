(defpackage :vico-term.util
  (:use :cl)
  (:export #:wide-character-width
           #:get-terminal-dimensions
           #:setup-terminal-input #:restore-terminal-input
           #:read-terminal-event))

(defpackage :vico-term.impl
  (:use #:alexandria
        #:vico-lib ; XXX export & remove vico-lib
        #:vico-lib.ui
        #:vico-lib.evloop
        #:vico-lib.key-event)
  (:local-nicknames (:term :vico-term.util))
  (:export #:tui #:tui-window
           #:%tui-redisplay))

(defpackage :vico-term
  (:use #:alexandria
        #:vico-lib
        #:vico-lib.evloop
        #:vico-term.impl)
  (:local-nicknames (:concurrency :vico-lib.concurrency)
                    (:ui          :vico-lib.ui)
                    (:term        :vico-term.util))
  (:export #:main))
