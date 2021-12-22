# A text editor

This project aims to be a extensible and performant editor and scripting environment.
Implementations supported will include SBCL, CCL and ECL.

## Dependencies

Some lisp dependencies are not currently on quicklisp:
- https://github.com/Zulu-Inuoe/raw-bindings-sdl2 (thanks Zulu)
- https://github.com/Zulu-Inuoe/raw-bindings-sdl2-ttf (thanks Zulu)

Unfortunately at present some C dependencies are necessary at present to save some time.
Perhaps someday they will be rewritten in lisp:

- [SDL2](https://www.libsdl.org/download-2.0.php) (main frontend)
- [SDL2-ttf](https://www.libsdl.org/projects/SDL_ttf)
- [tree-sitters](https://github.com/plisp/tree-sitters) (optional, unused atm)
- [pcre2](https://github.com/PhilipHazel/pcre2) (optional, unused atm)
- [libst](https://github.com/Plisp/libst.git) (optional, unused atm)

## Build (TODO update)

Run with `(vico-sdl:main FILENAME)` or dump into a executable (in the current working directory) with
```
sbcl --eval "(ql:quickload :vico-term)" \
--eval "(uiop:register-image-restore-hook #'vico-term:main nil)" \
--eval "(uiop:dump-image (concatenate 'string (namestring (uiop:getcwd)) \"vico\") :executable t)"
```

## todo

The first priority is to support its own development.

- [ ] editing
- [ ] indented line wrap
- [ ] auto-indent
- [ ] search
- [ ] bracket matching/wrangling
- [ ] basic syntax highlighting + color themes
- [ ] REPL
- [ ] debugger
- [ ] goto definition using sly
- [ ] completion, snippets
- [ ] smooth scroll with momentum
- [ ] regex with pcre2partial
