/* Based on MiniScheme (original credits follow)
 *
 * coded by Atsushi Moriwaki (11/5/1989)
 * E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 * This version has been modified by R.C. Secrist.
 * 
 * Mini-Scheme is now maintained by Akira KIDA.
 * 
 * This is a revised and modified version by Akira KIDA.
 * current version is 0.85k4 (15 May 1994)
 *
 * THIS SOFTWARE IS IN THE PUBLIC DOMAIN
 * ------------------------------------
 * This software is completely free to copy, modify and/or re-distribute.
 * But I would appreciate it if you left my name on the code as the author.
 */


%token_prefix TOK_
%token_type { cell* }
%extra_argument { cell **result }
%token_destructor { (void)(result); }
%parse_failure { error("Parser failure..."); }

%include {
#include <assert.h>
#include "yocto.h"
extern cell *NIL;
extern cell *QUOTE;
extern cell *QQUOTE;
extern cell *UNQUOTE;
extern cell *UNQUOTESP;
}

expr                ::= datums(e).                            { *result = e; }

datums(lhs)         ::= datum(e).                             { lhs = cons(e, NIL); }
datums(lhs)         ::= datums(a) datum(b).                   { lhs = list_append(a, cons(b, NIL)); }

datum(lhs)          ::= simple_datum(e).                      { lhs = e; }
datum(lhs)          ::= compound_datum(e).                    { lhs = e; }

simple_datum(lhs)   ::= BOOLEAN(e).                           { lhs = e; }
simple_datum(lhs)   ::= NUMBER(e).                            { lhs = e; }
simple_datum(lhs)   ::= CHARACTER(e).                         { lhs = e; }
simple_datum(lhs)   ::= STRING(e).                            { lhs = e; }
simple_datum(lhs)   ::= SYMBOL(e).                            { lhs = e; }

compound_datum(lhs) ::= list(e).                              { lhs = e; }
compound_datum(lhs) ::= vector(e).                            { lhs = e; }

list(lhs)           ::= LPAREN RPAREN.                        { lhs = NIL; }
list(lhs)           ::= LPAREN datums(e) RPAREN.              { lhs = e; }
list(lhs)           ::= LPAREN datums(a) DOT datum(b) RPAREN. { lhs = list_append(a, b); }
list(lhs)           ::= abbrev_prefix(a) datum(b).            { lhs = cons(a, cons(b, NIL)); }

abbrev_prefix(lhs)  ::= SQUOTE.                               { lhs = QUOTE; }
abbrev_prefix(lhs)  ::= BACKQUOTE.                            { lhs = QQUOTE; }
abbrev_prefix(lhs)  ::= COMMA.                                { lhs = UNQUOTE; }
abbrev_prefix(lhs)  ::= COMMA_AT.                             { lhs = UNQUOTESP; }

vector(lhs)         ::= VECTOR RPAREN.                        { lhs = mk_vector(0, NIL); }
vector(lhs)         ::= VECTOR datums(e) RPAREN.              { lhs = mk_vector(list_length(e), e); }

