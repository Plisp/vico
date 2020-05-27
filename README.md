# A text editor in progress
Run with `(vico-term:main FILENAME)`

This project aims to be a performant, extensible single user editor to replace emacs.
Implementations supported will include SBCL, CCL and ECL.

## TODO

- [x] fix text deletion
- [x] rewrite redisplay for editing
- [x] rewrite buffer interface and add error handling
- [ ] implement interface: use bytes and mmap() in buffer backend
- [ ] implement file saving - easy
- [ ] incremental search - easy
- [ ] implement buffer collapse - easy
- [ ] clipboard - easy
- [ ] timers - easy
- [ ] selections?
- [ ] multiline highlighting & bracket matching
- [ ] write proper terminal key/mouse/sequence abstractions, split into library
- [ ] SDL2 frontend pls
- [ ] auto-indent
- [ ] split out language support, guess by extension
- [ ] configuration file
- [ ] async/threading interface
- [ ] file watching using `entr`
- [ ] windowing and keybindings
- [ ] autocompletion, think about interaction headache with snippets which some people use
