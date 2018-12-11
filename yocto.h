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

#ifndef YOCTO_H
#define YOCTO_H

#include <stdio.h>

/* #define VERBOSE */   /* define this if you want verbose GC */

#ifndef PROMPT
#define PROMPT "> "
#endif

#ifndef LINESIZE
#define LINESIZE 4096
#endif

char strbuf[LINESIZE];

/* cell structure */
typedef struct cell {
    unsigned short _flag;
    union {
        struct {
            struct cell *_car;
            struct cell *_cdr;
        } _cons;
        struct {
            char *_svalue;
            short _keynum;
        } _string;
        struct {
            long   _ivalue;
            double _rvalue;
        } _number;
        struct {
            char _in;
            FILE *_file;
        } _port;
    };
} cell;

cell *mk_symbol(const char*);
cell *mk_string(const char*);
cell *mk_character(char);
cell *mk_exact(long);
cell *mk_inexact(double);
cell *mk_vector(int, cell*);

cell *cons(cell*, cell*);
cell *list_append(cell*, cell*);
int list_length(cell*);

void error(const char *);
void fatal_error(const char *);

char *line;    /* input buffer */

FILE *infp;    /* input file */
FILE *outfp;   /* output file */

char is_interactive;
/*
typedef enum {
    TOK_EOF,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_DOT,
    TOK_ATOM,
    TOK_QUOTE,
    TOK_BQUOTE,
    TOK_COMMA,
    TOK_ATMARK,
    TOK_VECTOR,
} token;
*/

int lex(cell**);
void clearinput();
void flushinput();

#endif /* YOCTO_H */
