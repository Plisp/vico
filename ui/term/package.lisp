(defpackage :vico-term.util
  (:use :cl :alexandria)
  (:local-nicknames (:ffi :cffi)
                    (:error :vico-core.conditions))
  (:export #:character-width
           #:get-terminal-dimensions
           #:setup-terminal-input #:restore-terminal-input
           #:read-terminal-event))

(defpackage :vico-term.impl
  (:use :cl
        :alexandria
        :vico-core.ui
        :vico-core.evloop
        :vico-core.key-event)
  (:local-nicknames (:ffi :cffi)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:hl :vico-core.syntax-highlighting)
                    (:term :vico-term.util)
                    (:graphemes :vico-core.graphemes)
                    (:buf :vico-core.buffer)
                    (:stdbuf :vico-core.standard-buffer))
  (:export #:tui #:tui-window
           #:%tui-redisplay))

(defpackage :vico-term
  (:use :cl
        :alexandria
        :vico-core.evloop ;TODO remove
        :vico-term.impl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:buf         :vico-core.buffer)
                    (:ui          :vico-core.ui)
                    (:term        :vico-term.util))
  (:export #:main))
