(defpackage :vico-term.bindings
  (:use :cl :alexandria)
  (:local-nicknames (:enc :babel-encodings)
                    (:ui :vico-core.ui)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:hl :vico-core.highlight)
                    (:term :uncursed)
                    (:graphemes :vico-core.graphemes)
                    (:ed :vico-core.command-loop)
                    (:buf :vico-core.buffer)
                    (:log :vico-lib.logging))
  (:export #:lookup-binding))

(defpackage :vico-term.impl
  (:use :cl :alexandria
        :vico-core.ui)
  (:local-nicknames (:enc :babel-encodings)
                    (:conditions :vico-core.conditions)
                    (:concurrency :vico-core.concurrency)
                    (:hl :vico-core.highlight)
                    (:term :uncursed)
                    (:graphemes :vico-core.graphemes)
                    (:ed :vico-core.command-loop)
                    (:buf :vico-core.buffer)
                    (:bindings :vico-term.bindings)
                    (:log :vico-lib.logging))
  (:export #:tui #:tui-window))

(defpackage :vico-term
  (:use :cl :alexandria
        :vico-term.impl)
  (:local-nicknames (:concurrency :vico-core.concurrency)
                    (:buf :vico-core.buffer)
                    (:ed :vico-core.command-loop)
                    (:ui :vico-core.ui)
                    (:term :uncursed))
  (:export #:dmain #:main))
