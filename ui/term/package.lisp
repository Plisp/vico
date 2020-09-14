(defpackage :vico-term.impl
  (:use :cl :alexandria
        :vico-core.ui)
  (:local-nicknames (:enc :babel-encodings)
                    (:ffi :cffi)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:hl :vico-core.highlight)
                    (:term :uncursed)
                    (:graphemes :vico-core.graphemes)
                    (:ev :vico-core.evloop)
                    (:key :vico-core.key-event)
                    (:buf :vico-core.buffer))
  (:export #:tui #:tui-window))

(defpackage :vico-term
  (:use :cl :alexandria
        :vico-term.impl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:buf :vico-core.buffer)
                    (:ev :vico-core.evloop)
                    (:ui :vico-core.ui)
                    (:term :uncursed))
  (:export #:dmain #:main))
