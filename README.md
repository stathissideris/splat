# splat

Lisp-like syntax to C transpiler

## Related work

* [Cgen](http://www.european-lisp-symposium.org/editions/2014/selgrad.pdf),
  also see p88 in
  [this paper](http://www.european-lisp-symposium.org/editions/2014/ELS2014.pdf).
* [Clasp](https://github.com/drmeister/clasp)
* [Amplify](http://voodoo-slide.blogspot.co.uk/2010/01/amplifying-c.html). [On Hacker News](https://news.ycombinator.com/item?id=11054089).
  [code](https://github.com/deplinenoise/c-amplify)
* [Embeddable Common Lisp](https://common-lisp.net/project/ecl/) is an
  efficient implementation of Lisp that compiles to C. It can be
  easily used to interface C and Lisp.
* [Terra](http://terralang.org/) is a new low-level system programming
  language that is designed to interoperate seamlessly with the Lua
  programming language.
* [Lua FFI](http://luajit.org/ext_ffi.html) parses plain C declarations.
* [Cyclone](http://cyclone.thelanguage.org/) is a safe dialect of C
  (unsupported).

### Somehow in the Clojure ecosystem
* [Rhine](https://github.com/artagnon/rhine-ml) a Clojure-inspired
  language on LLVM. GH page explains how to implement closures, macros
  etc.
* [mjolnir](https://github.com/halgari/mjolnir) is a Clojure library
  designed to simplify native code generation.
* [clojure-metal](https://github.com/halgari/clojure-metal)
* [cljs-terra](https://github.com/ohpauleez/cljs-terra)

## Reading

* [LLVM IR is better than assembly](https://idea.popcount.org/2013-07-24-ir-is-better-than-assembly/)
* [This thread](https://groups.google.com/forum/#!topic/clojure-dev/bex25u9hWIw)
* [Object-Oriented Programming With ANSI-C](https://www.cs.rit.edu/~ats/books/ooc.pdf) (PDF)
* [LLVM Language Reference Manual](http://llvm.org/docs/LangRef.html)

## License

Copyright © 2016 Stathis Sideris

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
