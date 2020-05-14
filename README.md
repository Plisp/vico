# A text editor in progress
Run with `(vico-term:main FILENAME)`

This project aims to be a performant, extensible single user editor to replace emacs.
Implementations supported will include SBCL, CCL and ECL.
Some lessons have been learnt, a rewrite of the buffer will be done.

## TODO

- [x] ~~fix text deletion~~
- [x] ~~rewrite redisplay for editing~~
- [ ] rewrite buffer
- [ ] write proper terminal key/mouse/sequence abstractions, split into library
- [ ] implement file saving - easy
- [ ] incremental search - easy
- [ ] implement buffer collapse - easy
- [ ] copy/paste - easy
- [ ] undo/redo - undo struct - no copy
- [ ] selections?
- [ ] multiline highlighting & bracket matching
- [ ] auto-indent
- [ ] split out language support, guess by extension
- [ ] configuration file
- [ ] async/threading interface
- [ ] file watching using `entr`
- [ ] implement windowing bindings
- [ ] autocompletion, think about interaction headache with snippets which some use
