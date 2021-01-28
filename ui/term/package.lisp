(defpackage :vico-term.bindings
  (:use :cl :alexandria)
  (:local-nicknames (:ui :vico-core.ui)
                    (:graphemes :vico-core.graphemes)
                    (:ed :vico-core.editor)
                    (:buf :vico-core.buffer)
                    (:cmd :vico-lib.commands)
                    (:log :vico-lib.logging))
  (:export #:lookup-binding))

(defpackage :vico-term.impl
  (:use :cl :alexandria
        :vico-core.ui)
  (:local-nicknames (:conditions :vico-core.conditions)
                    (:hl :vico-core.highlight)
                    (:term :uncursed)
                    (:sys :uncursed-sys)
                    (:ed :vico-core.editor)
                    (:buf :vico-core.buffer)
                    (:bindings :vico-term.bindings)
                    (:log :vico-lib.logging))
  (:export #:tui #:tui-window))

(defpackage :vico-term
  (:use :cl :alexandria
        :vico-term.impl)
  (:local-nicknames (:buf :vico-core.buffer)
                    (:ed :vico-core.editor)
                    (:ui :vico-core.ui)
                    (:term :uncursed))
  (:export #:dmain #:main))
