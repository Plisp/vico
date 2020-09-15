# A text editor in progress
Run with `(vico-term:dmain FILENAME)`
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
- [ ] bracket matching - easy
- [ ] search - easy
- [ ] handle windowing and resizing
- [ ] optionally use tree-sitter (plugin)
- [ ] autocompletion, work out interaction with snippets which some like
- [ ] clipboard - easy
- [ ] timers - easy
- [ ] SDL2 frontend pls
- [ ] auto-indent
- [ ] configuration file - easy
- [ ] async/threading interface
- [ ] file watching using `entr` - easy
