# Fez

A small Scheme-like LISP written in Clojure, based heavily on chapter 4 of the Structure and Interpretation of Computer Programs (SICP). Full text here:  http://mitpress.mit.edu/sicp/full-text/book/book.html  

There is also a lecture on this topic by Gerald Sussman as part of the SICP course available here: http://www.youtube.com/watch?v=0m6hoOelZH8 and transcription of the code he wrote is in lec7.scm in the main src folder.

You can see a working Scheme-in-Scheme version at github.com/CJOlsen/scheme-in-scheme which is *much* closer to the original SICP text.


## Current State of the Program

The Scheme-like LISP is still *very* minimal. Fez is intended for educational and entertainment purposes only - in other words this is just for fun.  

The current frame/environment setup hasn't been tested very thoroughly and it's best to assume it will break.  

When changing the program restarting the REPL solves half the problems that show up.


## Usage

2 ways:

1. Using Leiningen
   - 'lein run' from inside the project directory

2. Using Emacs (with nREPL already installed)
   - open core.clj from fez/src/fez/core.clj
   - start inferior lisp (C-c C-z)
   - select entire file (C-x h)
   - evaluate selection (C-c C-r)
   - repeat last step if there are errors, the order of fn definition isn't perfect
   - in the repl evaluate this: (driver-loop)
   - that should start a mini-Scheme buffer within nREPL
   * if the above commands don't make sense you might want to find an Emacs tutorial to work through.  It's worth it.


## Extending the language

There's a function called "primitive-procedures" that currently looks like this, though it will change in the future:

(def primitive-procedures
  [(list 'car first) 
   (list 'cdr rest)
   (list 'cons cons)
   (list 'null nil)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '/ /)
   (list 'about-fez about-fez)
   (list 'sq square)
   (list 'cube (fn [x] (* x x x)))
   ;;<more primitives>
   ])

The list syntax is a vestige from the Scheme-in-Scheme of SICP, but if you want to add something this is the place to do it.  You can map to a Clojure built-in or any function you define in the file.  You can also add lambda functions inline, as shown in the "cube" function.  The sky's the limit.



## License

Copyright © 2013 Christopher Olsen

Distributed under the GNU GPLv3 license

(if for some reason you'd appreciate this under a weaker copyleft feel free to ask, can't promise anything but I'm open to it)
