## Yocto Scheme

An over-engineered port of MiniScheme aiming for R4RS compliance but mostly
just a playground to learn how to put together a working lexer/parser stack.

* re2c for lexing
* lemon for parsing
* linenoise for command line history/editing.

### Credits

    Mini-Scheme Interpreter Version 0.85
    
    coded by Atsushi Moriwaki (11/5/1989)
    E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
    This version has been modified by R.C. Secrist.
  
    Mini-Scheme is now maintained by Akira KIDA.

    This is a revised and modified version by Akira KIDA.
    current version is 0.85k4 (15 May 1994)

    THIS SOFTWARE IS IN THE PUBLIC DOMAIN
  
    This software is completely free to copy, modify and/or re-distribute.
    But I would appreciate it if you left my name on the code as the author.

Some (a lot of) code ported from TinyScheme

lcm & gcd in init.scm from stalin 

### Compile Time Dependencies

re2c >= 1.0.3

lemon parser generator source is in tree

linenoise included as a git submodule

### Known Bugs

- [ ] quasiquote macro doesn't work inside a vector
- [ ] #| comments |# overflow the input buffer

### TODO

#### 6.5.5. Numerical operations
- [ ] numerator
- [ ] denominator
- [ ] rationalize
- [ ] make-rectangular
- [ ] make-polar
- [ ] real-part
- [ ] imag-part
- [ ] magnitude
- [ ] angle

#### 6.7. Strings
- [ ] string-set!
- [ ] string=?
- [ ] string-ci=?
- [ ] string<?
- [ ] string>?
- [ ] string<=?
- [ ] string>=?
- [ ] string-ci<?
- [ ] string-ci>?
- [ ] string-ci<=?
- [ ] string-ci>=?
- [ ] substring
- [ ] string-append
- [ ] string->list
- [ ] list->string
- [ ] string-copy
- [ ] string-fill!

#### 6.10.1. Ports
- [ ] call-with-input-file
- [ ] call-with-output-file
- [ ] with-input-from-file
- [ ] with-output-to-file
- [ ] open-input-file
- [ ] open-output-file
- [ ] close-input-file
- [ ] close-output-file

#### 6.10.2. Input
- [ ] read-char
- [ ] peek-char
- [ ] eof-object?
- [ ] char-ready?

#### 6.10.4. System interface
- [ ] transcript-on
- [ ] transcript-off

