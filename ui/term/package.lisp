(defpackage :vico-term.util
  (:use :cl)
  (:export #:character-width
           #:get-terminal-dimensions
           #:setup-terminal-input #:restore-terminal-input
           #:read-terminal-event))

(defpackage :vico-term.impl
  (:use #:alexandria
        #:vico-lib ;XXX remove
        #:vico-core.ui
        #:vico-core.evloop
        #:vico-core.key-event)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:hl :vico-core.syntax-highlighting)
                    (:term :vico-term.util)
                    (:buf :vico-core.buffer))
  (:export #:tui #:tui-window
           #:%tui-redisplay))

(defpackage :vico-term
  (:use #:alexandria
        #:vico-core.evloop
        #:vico-lib ;XXX remove
        #:vico-term.impl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:ui          :vico-core.ui)
                    (:term        :vico-term.util))
  (:export #:main))
