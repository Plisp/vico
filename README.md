# A text editor in progress
Run with `(vico-term:dmain FILENAME)` from a remote slime connection (slime-connect)
or dump into a executable (in the current working directory) with
```
sbcl --eval "(ql:quickload :vico-term)" \
--eval "(uiop:register-image-restore-hook #'vico-term:main nil)" \
--eval "(uiop:dump-image (concatenate 'string (namestring (uiop:getcwd)) \"vico\") :executable t)"
```
This project aims to be a performant, extensible, single-user editor to replace emacs.
Implementations supported will include SBCL, CCL and ECL.

## TODO

- [x] fix text deletion
- [x] rewrite redisplay for editing
- [x] rewrite buffer interface and add error handling
- [x] implement interface: use bytes and mmap() in buffer backend
- [x] regex search - a bit tricky
- [x] write proper terminal abstractions, split into library
- [x] undo/redo - tricky
- [x] implement file saving - easy
- [x] ~~implement buffer collapse~~ - won't work
- [x] selections - easy
- [x] fix cl-ppcre edge cases, replace BMH matchers (they work now actually)
- [x] event system overhaul
- [ ] layout-based windowing (resizing is trivial)
- [ ] autocompletion, work out interaction with snippets
- [ ] bracket matching - easy
- [ ] search - easy
- [ ] clipboard - easy
- [ ] timers - easy
- [ ] auto-indent (lisp)
- [ ] mouse tracking?
- [ ] optionally use tree-sitter (plugin)
- [ ] SDL2 frontend
- [ ] configuration file - easy
- [ ] file watching using `entr` - easy
