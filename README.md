# A text editor in progress
![demo](https://user-images.githubusercontent.com/36055271/129137366-0cf5779e-8cb1-48f8-b272-b96ddc6294a8.gif)

Run with `(vico-sdl:main FILENAME)` or dump into a executable (in the current working directory) with
```
sbcl --eval "(ql:quickload :vico-term)" \
--eval "(uiop:register-image-restore-hook #'vico-term:main nil)" \
--eval "(uiop:dump-image (concatenate 'string (namestring (uiop:getcwd)) \"vico\") :executable t)"
```
This project aims to be a extensible and performant editor and scripting environment.
Implementations supported will include SBCL, CCL and ECL.

## Dependencies

Some lisp dependencies are not currently on quicklisp: polymorph.etc and SDL2 raw-bindings (thanks Zulu)
Unfortunately at present some C dependencies are necessary at present to save some time. Perhaps someday they will be rewritten in lisp:

- [pcre2](https://github.com/PhilipHazel/pcre2)
- [SDL2](https://www.libsdl.org/download-2.0.php)
- [SDL2-ttf](https://www.libsdl.org/projects/SDL_ttf)
- [tree-sitter](https://github.com/Plisp/tree-sitters.git)
- [libst](https://github.com/Plisp/libst.git)
