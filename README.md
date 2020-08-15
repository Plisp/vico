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
- [ ] bracket matching - easy
- [x] undo/redo - easy
- [ ] implement file saving - easy
- [x] regex search - a bit tricky
- [ ] search interface - easy
- [ ] implement buffer collapse - easy
- [ ] clipboard - easy
- [ ] timers - easy
- [ ] selections - easy
- [ ] multiline highlighting
- [ ] split out language support, guess by extension - easy
- [ ] split lexers out from actual highlighting, use elsewhere
- [ ] do SIGWINCH handling right
- [ ] write proper terminal key/mouse/sequence abstractions, split into library
- [ ] SDL2 frontend pls
- [ ] auto-indent
- [ ] configuration file - easy
- [ ] async/threading interface
- [ ] file watching using `entr` - easy
- [ ] windowing and keybindings (think about tabbing impl)
- [ ] autocompletion, think about interaction headache with snippets which some people use
- [ ] optionally use tree-sitter (plugin)
