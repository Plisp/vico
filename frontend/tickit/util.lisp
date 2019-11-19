(in-package :vico-tickit.util)

(cffi:defcfun "wcwidth" :int
  (c wchar-t))

(defun wide-character-width (character)
  (wcwidth (char-code character)))
