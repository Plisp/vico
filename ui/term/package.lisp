(defpackage :vico-term.impl
  (:use :cl
   :alexandria
        :vico-core.ui
   :vico-core.evloop
        :vico-core.key-event)
  (:local-nicknames (:enc :babel-encodings)
                    (:ffi :cffi)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:hl :vico-core.syntax-highlighting)
                    (:term :uncursed)
                    (:graphemes :vico-core.graphemes)
                    (:buf :vico-core.buffer)
                    )
  (:export #:tui #:tui-window))

(defpackage :vico-term
  (:use :cl
        :alexandria
        :vico-core.evloop ;TODO remove
        :vico-term.impl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:buf :vico-core.buffer)
                    (:ui :vico-core.ui)
                    (:term :uncursed))
  (:export #:dmain #:main))
