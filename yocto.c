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

#include <ctype.h>
#include <limits.h>
#include <malloc.h>
#include <setjmp.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <locale.h>
#include <time.h>

#include "yocto.h"
#include "utf8.h"
#include "utf8-char-class.h"

#include "linenoise/linenoise.h"
#include "linenoise/encodings/utf8.h"

/*
 *  Basic memory allocation units
 */

#define CELL_SEGSIZE    4096    /* # of cells in one segment */
#define CELL_NSEGMENT   512     /* # of segments for cells */

#define InitFile "init.scm"
#define FIRST_CELLSEGS 10

#define T_CHARACTER      1 << 0     /* 0000000000000001 */
#define T_STRING         1 << 1     /* 0000000000000010 */
#define T_EXACT          1 << 2     /* 0000000000000100 */
#define T_INEXACT        1 << 3     /* 0000000000001000 */
#define T_SYMBOL         1 << 4     /* 0000000000010000 */
#define T_SYNTAX         1 << 5     /* 0000000000100000 */
#define T_PROC           1 << 6     /* 0000000001000000 */
#define T_PAIR           1 << 7     /* 0000000010000000 */
#define T_VECTOR         1 << 8     /* 0000000100000000 */
#define T_CLOSURE        1 << 9     /* 0000001000000000 */
#define T_CONTINUATION   1 << 10    /* 0000010000000000 */
#define T_MACRO          1 << 11    /* 0000100000000000 */
#define T_PROMISE        1 << 12    /* 0001000000000000 */
#define T_PORT           1 << 13    /* 0010000000000000 */
#define T_ATOM           1 << 14    /* 0100000000000000 */  /* only for gc */
#define CLRATOM          49151      /* 1011111111111111 */  /* only for gc */
#define MARK             1 << 15    /* 1000000000000000 */
#define UNMARK           32767      /* 0111111111111111 */

/* macros for cell operations */
#define car(p)            ((p)->_cons._car)
#define cdr(p)            ((p)->_cons._cdr)
#define type(p)           ((p)->_flag)

#define isstring(p)       (type(p) & T_STRING)
#define string(p)         ((p)->_string)
#define strvalue(p)       str_ptr(((p)->_string))
#define keynum(p)         ((p)->_string._keynum)

#define isnumber(p)       (type(p) & T_EXACT || type(p) & T_INEXACT)

#define isexact(p)        (type(p) & T_EXACT || (type(p) & T_INEXACT && (double)ivalue(p) == rvalue(p)))
#define ivalue(p)         ((p)->_number._ivalue)

#define isinexact(p)      (type(p) & T_INEXACT)
#define rvalue(p)         ((p)->_number._rvalue)

#define ischar(p)         (type(p) & T_CHARACTER)
#define cvalue(p)         ((p)->_number._ivalue)

#define ispair(p)         (type(p) & T_PAIR)

#define isvector(p)       (type(p) & T_VECTOR)

#define issymbol(p)       (type(p) & T_SYMBOL)
#define symname(p)        strvalue(car(p))
#define hasprop(p)        (type(p) & T_SYMBOL)
#define symprop(p)        cdr(p)

#define issyntax(p)       (type(p) & T_SYNTAX)
#define isproc(p)         (type(p) & T_PROC)
#define syntaxname(p)     strvalue(car(p))
#define syntaxnum(p)      car(p)->_op
#define procnum(p)        ivalue(p)

#define isclosure(p)      (type(p) & T_CLOSURE)
#define ismacro(p)        (type(p) & T_MACRO)
#define closure_code(p)   car(p)
#define closure_env(p)    cdr(p)

#define iscontinuation(p) (type(p) & T_CONTINUATION)
#define cont_dump(p)      cdr(p)

#define ispromise(p)      (type(p) & T_PROMISE)
#define setpromise(p)     type(p) |= T_PROMISE

#define isatom(p)         (type(p) & T_ATOM)
#define setatom(p)        type(p) |= T_ATOM
#define clratom(p)        type(p) &= CLRATOM

#define isport(p)         (type(p) & T_PORT)
#define isinput(p)        ((p)->_port._in)
#define port(p)           ((p)->_port._file)

#define ismark(p)         (type(p) & MARK)
#define setmark(p)        type(p) |= MARK
#define clrmark(p)        type(p) &= UNMARK

#define caar(p)           car(car(p))
#define cadr(p)           car(cdr(p))
#define cdar(p)           cdr(car(p))
#define cddr(p)           cdr(cdr(p))
#define cadar(p)          car(cdr(car(p)))
#define caddr(p)          car(cdr(cdr(p)))
#define cdaar(p)          cdr(car(car(p)))
#define cadaar(p)         car(cdr(car(car(p))))
#define cadddr(p)         car(cdr(cdr(cdr(p))))
#define cddddr(p)         cdr(cdr(cdr(cdr(p))))

#define istrue(p)         ((p) != F)
#define isfalse(p)        ((p) == F)

/* arrays for segments */
static cell *cell_seg[CELL_NSEGMENT];
static int   last_cell_seg = -1;

/* We use 4 registers. */
static cell *args;                            /* register for arguments of function */
static cell *envir;                           /* stack register for current environment */
static cell *code;                            /* register for current code */
static cell *dump;                            /* stack register for next evaluation */

/* global pointers to special symbols */

static cell _NIL;
cell *NIL = &_NIL;                            /* special cell representing empty cell */
static cell _T;
cell *T = &_T;                                /* special cell representing #t */
static cell _F;
cell *F = &_F;                                /* special cell representing #f */
static cell *oblist = &_NIL;                  /* pointer to symbol table */
static cell *strlist = &_NIL;                 /* pointer to string list */
static cell *global_env;                      /* pointer to global environment */

cell *QUOTE;                                  /* pointer to symbol quote */
cell *QQUOTE;                                 /* pointer to symbol quasiquote */
cell *UNQUOTE;                                /* pointer to symbol unquote */
cell *UNQUOTESP;                              /* pointer to symbol unquote-splicing */
static cell *LAMBDA;                          /* pointer to symbol lambda */
static cell *FEED_TO;                         /* pointer to symbol => */

static cell *free_cell = &_NIL;               /* pointer to top of free cells */
static long fcells = 0;                       /* # of free cells */

static char gc_verbose;                       /* if gc_verbose is not zero, print gc status */

static jmp_buf error_jmp;

static void *parser;

void Parse(void*, int, cell*, cell**);
void *ParseAlloc(void *(*mallocProc)(size_t));
void ParseFree(void *, void (*freeProc)(void*));

char is_interactive;

FILE *infp;    /* input file */
FILE *outfp;   /* output file */
str in_filename;

char strbuf[LINESIZE];

/* ========== garbage collector ========== */

/*--
 *  We use algorithm E (Kunuth, The Art of Computer Programming Vol.1, sec.3.5) for marking.
 */
static void mark(cell *a)
{
    cell *t, *q, *p;

//E1:
    t = NULL;
    p = a;
E2:
    setmark(p);
//E3:
    if(isvector(p)) {
        int i, num = ivalue(p) / 2 + ivalue(p) % 2;
        for (i = 0; i < num; i++) {
            /* Vector cells will be treated like ordinary cells */
            mark(p + 1 + i);
        }
    }

    if (isatom(p))
        goto E6;
//E4:
    q = car(p);
    if (q && !ismark(q)) {
        setatom(p);
        car(p) = t;
        t = p;
        p = q;
        goto E2;
    }
E5:
    q = cdr(p);
    if (q && !ismark(q)) {
        cdr(p) = t;
        t = p;
        p = q;
        goto E2;
    }
E6:
    if (!t)
        return;
    q = t;
    if (isatom(q)) {
        clratom(q);
        t = car(q);
        car(q) = p;
        p = q;
        goto E5;
    } else {
        t = cdr(q);
        cdr(q) = p;
        p = q;
        goto E6;
    }
}

/* garbage collection. parameter a, b is marked. */
static void gc(cell *a, cell *b)
{
    cell *p;
    short i;
    long j;

    if (gc_verbose)
        printf("gc...");

    /* mark system globals */
    mark(oblist);
    mark(strlist);
    mark(global_env);

    /* mark current registers */
    mark(args);
    mark(envir);
    mark(code);
    mark(dump);

    /* mark variables a, b */
    mark(a);
    mark(b);

    /* garbage collect */
    clrmark(NIL);
    fcells = 0;
    free_cell = NIL;
    for (i = 0; i <= last_cell_seg; i++) {
        for (j = 0, p = cell_seg[i]; j < CELL_SEGSIZE; j++, p++) {
            if (ismark(p)) {
                clrmark(p);
            } else {
                type(p) = 0;
                cdr(p) = free_cell;
                car(p) = NIL;
                free_cell = p;
                ++fcells;
            }
        }
    }

    if (gc_verbose)
        printf(" done %ld cells are recovered.\n", fcells);
}

/* allocate new cell segment */
static int alloc_cellseg(int n)
{
    cell *p;
    long i;
    int k;

    for (k = 0; k < n; k++) {
        if (last_cell_seg >= CELL_NSEGMENT - 1)
            return k;

        p = (cell*) malloc(CELL_SEGSIZE * sizeof(cell));

        if (p == NULL)
            return k;

        cell_seg[++last_cell_seg] = p;
        fcells += CELL_SEGSIZE;

        for (i = 0; i < CELL_SEGSIZE - 1; i++, p++) {
            type(p) = 0;
            car(p) = NIL;
            cdr(p) = p + 1;
        }

        type(p) = 0;
        car(p) = NIL;
        cdr(p) = free_cell;

        free_cell = cell_seg[last_cell_seg];
    }

    return n;
}

/* search objlist by name */
static cell *oblist_get_name(const char *name)
{
    cell *x;

    for (x = oblist; x != NIL; x = cdr(x))
        if (str_eq(str_ref(name), string(caar(x))))
            return x;

    return NIL;
}

/* search strlist by name */
static cell *strlist_get_name(const char *name)
{
    cell **x= &strlist;
    str n = str_ref(name);

    while (*x != NIL && !str_eq(n, string(car(*x)))) {
        x = &cdr(*x);
    }

    return *x == NIL ? NIL : car(*x);
}

/* get new cell.  parameter a, b is marked by gc. */
static cell *get_cell(cell *a, cell *b)
{
    cell *x;

    if (free_cell == NIL) {
        gc(a, b);

        if (free_cell == NIL) {
            if (!alloc_cellseg(1)) {
                args = envir = code = dump = NIL;
                gc(NIL, NIL);

                if (free_cell != NIL)
                    error("run out of cells --- return to top level");
                else
                    fatal_error("run out of cells --- unable to recover cells");
            }
        }
    }

    x = free_cell;
    free_cell = cdr(x);
    --fcells;

    return x;
}

/* get new cons cell */
cell *cons(cell *a, cell *b)
{
    cell *x = get_cell(a, b);

    type(x) = T_PAIR;
    car(x) = a;
    cdr(x) = b;
    return x;
}

static int count_consecutive_cells(cell *x, int needed) {
    int n;

    for (n = 1; cdr(x) == x + 1; x = cdr(x), n++) {
        if (n > needed)
            return n;
    }

    return n;
}

static cell *find_consecutive_cells(int needed)
{
    int cnt;
    cell **pp = &free_cell;

    while (*pp != NIL) {
        cnt = count_consecutive_cells(*pp, needed);

        if (cnt >= needed) {
            cell *x = *pp;
            *pp = cdr(*pp + needed - 1);
            fcells -= needed;
            return x;
        }

        pp = &cdr(*pp + cnt - 1);
    }

    return NIL;
}

static cell *get_consecutive_cells(int needed)
{
    cell *x;

    /* Are there any cells available? */
    x = find_consecutive_cells(needed);

    if (x != NIL) { 
        return x;
    }

    /* If not, try gc'ing some */
    gc(NIL, NIL);

    x = find_consecutive_cells(needed);

    if (x != NIL) { 
        return x;
    }

    /* If there still aren't, try getting more heap */
    if (!alloc_cellseg(1)) {
        args = envir = code = dump = NIL;

        if (free_cell != NIL)
            error("run out of cells --- return to top level");
        else
            fatal_error("run out of cells --- unable to recover cells");
    }

    return find_consecutive_cells(needed);
}

static void fill_vector(cell *vec, cell *obj) {
    int i, num = ivalue(vec) / 2 + ivalue(vec) % 2;
    for (i = 0; i < num; i++) {
        type(vec + 1 + i) = T_PAIR;
        car(vec + 1 + i) = obj;
        cdr(vec + 1 + i) = obj;
    }
}

static cell *vector_elem(cell *vec, int ielem) {
    int n = ielem / 2;
    if (ivalue(vec) == 0) {
        return NIL;
    } else if ( ielem % 2 == 0) {
        return car(vec + 1 + n);
    } else {
        return cdr(vec + 1 + n);
    }
}

static cell *set_vector_elem(cell *vec, int ielem, cell *a) {
    int n = ielem / 2;
    if (ielem % 2 == 0) {
        return car(vec + 1 + n) = a;
    } else {
        return cdr(vec + 1 + n) = a;
    }
}

cell *mk_vector(int len, cell *fill)
{
    int i = list_length(fill);
    int num = len / 2 + len % 2;
    cell *v = get_consecutive_cells(num + 1);

    if (v == NIL)
        error("couldn't allocate enough cells for vector --- return to top level");

    type(v) = (T_VECTOR | T_ATOM);
    ivalue(v) = len;

    if (i > 1) {
        for (i = 0; fill != NIL; fill = cdr(fill), i++)
            set_vector_elem(v, i, car(fill));
    } else {
        fill_vector(v, fill);
    }

    return v;
}

/* get number atom */
cell *mk_exact(long num)
{
    cell *x = get_cell(NIL, NIL);

    type(x) = (T_EXACT | T_ATOM);
    ivalue(x) = num;
    return x;
}

/* get number atom */
cell *mk_inexact(double num)
{
    cell *x = get_cell(NIL, NIL);

    type(x) = (T_INEXACT | T_ATOM);
    rvalue(x) = num;
    return x;
}

/* get character atom */
cell *mk_character(char32_t c)
{
    cell *x = get_cell(NIL, NIL);

    type(x) = (T_CHARACTER | T_ATOM);
    cvalue(x) = c;
    return x;
}

/* get new string */
cell *mk_string(const char *s)
{
    cell *x = strlist_get_name(s);

    if (x != NIL) {
        return x;
    } else {
        x = get_cell(NIL, NIL);

        str_cpy(&string(x), str_ref(s));
        type(x) = (T_STRING | T_ATOM);
        strlist = cons(x, strlist);
        return x;
    }
}

/* len is the length for the empty string in characters */
static cell *mk_empty_string(int len, char32_t fill)
{
    int i, offset = 0;
/*
    for (i = 0; i < len; i++)
        strbuf[i] = fill;
    strbuf[len] = '\0';
*/
    for (i = 0; i < len; i++)
        offset += u8_wc_toutf8(strbuf + offset, fill);
    strbuf[offset] = '\0';

    return mk_string(strbuf);
}

/* get new symbol */
cell *mk_symbol(const char *name)
{
    cell *x = oblist_get_name(name);

    if (x != NIL) {
        return car(x);
    } else {
        x = cons(mk_string(name), NIL);
        type(x) = T_SYMBOL;
        oblist = cons(x, oblist);
        return x;
    }
}

static void mk_syntax(unsigned short op, const char *name)
{
    cell *x;

    x = cons(mk_string(name), NIL);
    type(x) = (T_SYNTAX | T_SYMBOL);
    syntaxnum(x) = op;
    oblist = cons(x, oblist);
}

static cell *mk_string_itoa(long number, int base) {
    static const char *str = "0123456789abcdef";
    char *p = &strbuf[LINESIZE - 1];

    long num = abs(number);

    *p = '\0';

    do {
        *--p = str[num % base];
        num /= base;
    } while (num);

    if (number < 0) {
        *--p = '-';
    }

    return mk_string(p);
}

static void mk_proc(unsigned short op, const char *name)
{
    cell *x, *y;

    x = mk_symbol(name);
    y = get_cell(NIL, NIL);
    type(y) = (T_PROC | T_ATOM);
    ivalue(y) = (long) op;
    car(global_env) = cons(cons(x, y), car(global_env));
}

static cell *mk_port(FILE *f, char in)
{
    cell *x = get_cell(NIL, NIL);

    type(x) = (T_PORT | T_ATOM);
    port(x) = f;
    isinput(x) = in;

    return x;
}

static cell *mk_gensym()
{
    static unsigned long gensym_cnt = 0;
    cell *x;
    char name[24];

    for(; gensym_cnt < ULONG_MAX; gensym_cnt++) {
        snprintf(name, 24, "gensym-%lx", gensym_cnt);

        x = oblist_get_name(name);

        if (x != NIL) {
            continue;
        } else {
            x = mk_symbol(name);
            return x;
        }
    }

    return NIL;
}

/* ========== Routines for Printing ========== */
#define    ok_abbrev(x)    (ispair(x) && cdr(x) == NIL)

static void strunquote(char *p, const char *s)
{
    *p++ = '"';
    for ( ; *s; ++s) {
        if (*s == '"') {
            *p++ = '\\';
            *p++ = '"';
        } else if (*s == '\n') {
            *p++ = '\\';
            *p++ = 'n';
        } else
            *p++ = *s;
    }
    *p++ = '"';
    *p = '\0';
}

/* print atoms */
static int printatom(cell *l, int f)
{
    int i;
    str p;
    
    if (l == NIL) {
        p = str_lit("()");
    } else if (l == T) {
        p = str_lit("#t");
    } else if (l == F) {
        p = str_lit("#f");
    } else if (isexact(l)) {
        sprintf(strbuf, "%ld", ivalue(l));
        p = str_ref(strbuf);
    } else if (isinexact(l)) {
        sprintf(strbuf, "%f", rvalue(l));
        p = str_ref(strbuf);
    } else if (isstring(l)) {
        if (!f) {
            p = string(l);
        } else {
            strunquote(strbuf, strvalue(l));
            p = str_ref(strbuf);
        }
    } else if (ischar(l)) {
        char32_t c = cvalue(l);
        if (!f) {
            strbuf[0] = c;
            strbuf[1] = 0;
            p = str_ref(strbuf);
        } else {
            switch (c) {
                case 0x07:
                    p = str_lit("#\\alarm");
                    break;
                case 0x08:
                    p = str_lit("#\\backspace");
                    break;
                case 0x5f:
                    p = str_lit("#\\delete");
                    break;
                case 0x1b:
                    p = str_lit("#\\escape");
                    break;
                case 0x0a:
                    p = str_lit("#\\newline");
                    break;
                case 0x00:
                    p = str_lit("#\\null");
                    break;
                case 0x0d:
                    p = str_lit("#\\return");
                    break;
                case 0x20:
                    p = str_lit("#\\space");
                    break;
                case 0x09:
                    p = str_lit("#\\tab");
                    break;
                default:
                    i = sprintf(strbuf, "#\\");
                    u8_toutf8(strbuf+i, LINESIZE - i, &c, 1);
                    p = str_ref(strbuf);
                    break;
            }

        }
    } else if (issymbol(l)) {
        p = str_ref(string(car(l)));
    } else if (isproc(l)) {
        cell *x = car(global_env);
        while(x != NIL) {
            if (l == cdar(x)) {
                x = caar(x);
                break;
            }
            x = cdr(x);
        }
        if (x != NIL) {
            p = string(car(x));
        } else {
            sprintf(strbuf, "#<PROCEDURE %ld>", procnum(l));
            p = str_ref(strbuf);
        }
    } else if (ismacro(l)) {
        p = str_lit("#<MACRO>");
    } else if (isclosure(l)) {
        p = str_lit("#<CLOSURE>");
    } else if (iscontinuation(l)) {
        p = str_lit("#<CONTINUATION>");
    } else if (isport(l) && isinput(l)) {
        p = str_lit("#<INPUT PORT>");
    } else if (isport(l) && !isinput(l)) {
        p = str_lit("#<OUTPUT PORT>");
    } else {
        error("load -- argument is not string");
        return 0;
    }

    if (f < 0)
        return str_len(p);

    str_cpy(outfp, p);

    return 0;
}

/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
static cell *mk_closure(cell *c, cell *e)
{
    cell *x = get_cell(c, e);

    type(x) = T_CLOSURE;
    car(x) = c;
    cdr(x) = e;
    return x;
}

/* make continuation. */
static cell *mk_continuation(cell *d)
{
    cell *x = get_cell(NIL, d);

    type(x) = T_CONTINUATION;
    cont_dump(x) = d;
    return x;
}

static cell *list_star(cell *d) {
    cell *p, *q;

    if (cdr(d) == NIL) {
        return car(d);
    }

    p = cons(car(d), cdr(d));
    q = p;

    while (cdr(cdr(p)) != NIL) {
        d = cons(car(p), cdr(p));
        if (cdr(cdr(p)) != NIL) {
            p = cdr(d);
        }
    }
    cdr(p) = car(cdr(p));
    return q;
}

/* reverse list -- make new cells */
static cell *reverse(cell *a)
{
    cell *p = NIL;

    while(ispair(a)) {
        p = cons(car(a), p);
        a = cdr(a);
    }

    return p;
}

/* reverse list --- no make new cells */
static cell *non_alloc_rev(cell *term, cell *list)
{
    cell *p = list, *result = term, *q;

    while (p != NIL) {
        q = cdr(p);
        cdr(p) = result;
        result = p;
        p = q;
    }
    return result;
}

/* append list -- produce new list */
static cell *revappend(cell *a, cell *b) {
    cell *result = a;
    cell *p = b;

    while (ispair(p)) {
        result = cons(car(p), result);
        p = cdr(p);
    }

    if (p == NIL) {
        return result;
    }

    return F;   /* signal an error */
}

/* append b to a -- doesn't check that a is a proper list */
static cell *tail_append(cell *a, cell *b)
{
    cell *p = a;
    while (cdr(p) != NIL)
        p = cdr(p);
    cdr(p) = cons(b, NIL);
    return a;
}

cell *list_append(cell *a, cell *b)
{
    cell *p = a;

    while (cdr(p) != NIL) {
        p = cdr(p);
    }
    cdr(p) = b;
    return a;
}

/* equivalence of atoms */
static int eqv(cell *a, cell *b)
{
    if (isstring(a) && isstring(b))
        return str_eq(string(a), string(b));
    else if (isexact(a) && isexact(b))
        return ivalue(a) == ivalue(b);
    else if (isinexact(a) && isinexact(b))
        return rvalue(a) == rvalue(b);
    else if (ischar(a) && ischar(b))
        return cvalue(a) == cvalue(b);
    else if (isproc(a) && isproc(b))
        return procnum(a) == procnum(b);
    return a == b;
}

static int equal(cell *a, cell *b)
{
    int i;
    if (ispair(a)) {
        return (ispair(b) && equal(car(a), car(b)) && equal(cdr(a), cdr(b)));
    } else if (isvector(a) && isvector(b) && ivalue(a) == ivalue(b)) {
        for (i = 0; i < ivalue(a); i++) {
            if (!equal(vector_elem(a, i), vector_elem(b, i)))
                return 0;
        }
        return 1;
    } else {
        return (!ispair(b) && eqv(a, b));
    }
}

#define num_ivalue(n) (isexact(n) ? ivalue(n) : (long)rvalue(n))
#define num_rvalue(n) (isexact(n) ? (double)ivalue(n) : rvalue(n))

static cell *num_div(cell *a, cell *b)
{
    if (isexact(a) && isexact(b) && ivalue(b) != 0 && ivalue(a) % ivalue(b) == 0) {
        return mk_exact(ivalue(a) / ivalue(b));
    } else if (isnumber(a) && isnumber(b)) {
        if (num_rvalue(b) != 0)
            return mk_inexact(num_rvalue(a) / num_rvalue(b));
    } else {
        error("Mathematical operation on non-number");
        return NULL;
    }
    error("Divide by zero");
    return NULL;
}

static cell *num_quo(cell *a, cell *b)
{
    // isnumber(a) && isnumber(b) checked before call
    if (isexact(a) && isexact(b) && ivalue(b) != 0) {
        return mk_exact(ivalue(a) / ivalue(b));
    } else if (num_rvalue(b) != 0) {
        return mk_inexact(num_rvalue(a) / num_rvalue(b));
    }
    error("Divide by zero");
    return NULL;
}

static cell *num_rem(cell *a, cell *b)
{
    // isnumber(a) && isnumber(b) checked before call
    if (isexact(a) && isexact(b) && ivalue(b) != 0) {
        return mk_exact(ivalue(a) % ivalue(b));
    } else if (num_rvalue(b) != 0) {
        return mk_inexact(remainder(num_rvalue(a), num_rvalue(b)));
    }
    error("Divide by zero");
    return NULL;
}

static cell *num_mod(cell *a, cell *b)
{
    long res;
    long e1 = num_ivalue(a);
    long e2 = num_ivalue(b);

    if (e2 == 0) {
        error("Divide by zero");
        return NULL;
    }

    res = e1 % e2;

    /* modulo should have same sign as second operand */
    if (res * e2 < 0) {
        res += e2;
    }

    return mk_exact(res);
}

/* Result is:
   proper list: length
   circular list: -1
   not even a pair: -2
   dotted list: -2 minus length before dot
*/
int list_length(cell *p)
{
    int i = 0;
    cell *slow, *fast;

    slow = fast = p;

    while (1) {
        if (fast == NIL)
            return i;

        if (!ispair(fast))
            return -2 - i;

        fast = cdr(fast);
        ++i;

        if (fast == NIL)
            return i;

        if (!ispair(fast))
            return -2 - i;

        ++i;
        fast = cdr(fast);

        slow = cdr(slow); /* Safe because we would have already returned if `fast' encountered a non-pair. */

        if (fast == slow) {
            /* the fast pointer has looped back around and caught up
               with the slow pointer, hence the structure is circular,
               not of finite length, and therefore not a list */
            return -1;
        }
    }
}

/* ========== Evaluation Cycle ========== */

/* operator code */
typedef enum {
    OP_LOAD,
    OP_T0LVL,
    OP_T1LVL,
    OP_EVAL,
    OP_E0ARGS,
    OP_E1ARGS,
    OP_APPLY,
    OP_DOMACRO,
    /* beginning of syntax */
    OP_QUOTE,
    OP_LAMBDA,
    OP_IF0,
    OP_IF1,
    OP_SET0,
    OP_SET1,
    OP_BEGIN,
    OP_COND0,
    OP_COND1,
    OP_AND0,
    OP_AND1,
    OP_OR0,
    OP_OR1,
    OP_CASE0,
    OP_CASE1,
    OP_CASE2,
    OP_LET0,
    OP_LET1,
    OP_LET2,
    OP_LET0AST,
    OP_LET1AST,
    OP_LET2AST,
    OP_LET0REC,
    OP_LET1REC,
    OP_LET2REC,
    OP_DELAY,
    OP_DEF0,
    OP_DEF1,
    /* end of syntax */
    OP_NOT,
    OP_BOOL,
    OP_EQV,
    OP_EQ,
    OP_EQUAL,
    OP_PAIR,
    OP_CONS,
    OP_CAR,
    OP_CDR,
    OP_SETCAR,
    OP_SETCDR,
    OP_NULL,
    OP_LISTP,
    OP_LIST,
    OP_LIST_LENGTH,
    OP_APPEND,
    OP_REVERSE,
    OP_LIST_TAIL,
    OP_LIST_REF,
    OP_MEMQ,
    OP_MEMV,
    OP_MEMBER,
    OP_ASSQ,
    OP_ASSV,
    OP_ASSOC,
    OP_SYMBOL,
    OP_SYMBOL_STRING,
    OP_STRING_SYMBOL,
    OP_NUMBER,
    OP_COMPLEX,
    OP_REAL,
    OP_RATIONAL,
    OP_INTEGER,
    OP_EXACT,
    OP_INEXACT,
    OP_NEQ,
    OP_LESS,
    OP_GRE,
    OP_LEQ,
    OP_GEQ,
    OP_ZEROP,
    OP_POSP,
    OP_NEGP,
    OP_ODD,
    OP_EVEN,
    OP_ADD,
    OP_MAX,
    OP_MIN,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_ABS,
    OP_QUOTIENT,
    OP_REMAINDER,
    OP_MODULO,
    OP_FLOOR,
    OP_CEILING,
    OP_TRUNCATE,
    OP_ROUND,
    OP_EXP,
    OP_LOG,
    OP_SIN,
    OP_COS,
    OP_TAN,
    OP_ASIN,
    OP_ACOS,
    OP_ATAN,
    OP_SQRT,
    OP_EXPT,
    OP_EXACT_INEXACT,
    OP_INEXACT_EXACT,
    OP_NUMBER_TO_STRING,
    OP_STRING_TO_NUMBER,
    OP_CHARP,
    OP_CHAR_EQ,
    OP_CHAR_LT,
    OP_CHAR_GT,
    OP_CHAR_LE,
    OP_CHAR_GE,
    OP_CHAR_CI_EQ,
    OP_CHAR_CI_LT,
    OP_CHAR_CI_GT,
    OP_CHAR_CI_LE,
    OP_CHAR_CI_GE,
    OP_CHAR_ALPHAP,
    OP_CHAR_NUMBERP,
    OP_CHAR_WSP,
    OP_CHAR_UPPERP,
    OP_CHAR_LOWERP,
    OP_CHAR_INT,
    OP_INT_CHAR,
    OP_CHAR_UPCASE,
    OP_CHAR_DOWNCASE,
    OP_STRINGP,
    OP_MAKE_STRING,
    OP_STRING,
    OP_STRING_LENGTH,
    OP_STRING_REF,
    OP_STRING_SET,
    OP_STRING_EQ,
    OP_STRING_CI_EQ,
    OP_STRING_LT,
    OP_STRING_CI_LT,
    OP_STRING_GT,
    OP_STRING_CI_GT,
    OP_STRING_LE,
    OP_STRING_CI_LE,
    OP_STRING_GE,
    OP_STRING_CI_GE,
    OP_SUBSTRING,
    OP_STRING_APPEND,
    OP_STRING_LIST,
    OP_LIST_STRING,
    OP_STRING_FILL,
    OP_STRING_NULL,
    // OP_STRING_JOIN,
    OP_STRING_COPY,
    OP_VECTORP,
    OP_MAKE_VECTOR,
    OP_VECTOR,
    OP_VECTOR_LENGTH,
    OP_VECTOR_REF,
    OP_VECTOR_SET,
    OP_VECTOR_LIST,
    OP_LIST_VECTOR,
    OP_VECTOR_FILL,
    OP_PROC,
    OP_PAPPLY,
    OP_FORCE,
    OP_SAVE_FORCED,
    OP_CONTINUATION,
    OP_INPUTP,
    OP_OUTPUTP,
    OP_CURRENT_IN_PORT,
    OP_CURRENT_OUT_PORT,
    OP_READ,
    OP_WRITE,
    OP_DISPLAY,
    OP_NEWLINE,
    OP_WRITE_CHAR,
    OP_LOCALE_SET,
    OP_CLOCK,
    OP_TIME,
    OP_TIME_STRING,
    OP_TIME_GMT,
    OP_SEED,
    OP_ROLL,
    OP_PEVAL,
    OP_ERR0,
    OP_ERR1,
    OP_GENSYM,
    OP_QUIT,
    OP_GC,
    OP_GCVERB,
    OP_NEWSEGMENT,
    OP_PUT,
    OP_GET,
    OP_PRINT_WIDTH,
    OP_P0_WIDTH,
    OP_P1_WIDTH,
    OP_GET_CLOSURE,
    OP_CLOSUREP,
    OP_MACROP,
    OP_0MACRO,
    OP_1MACRO,
    OP_RDSEXPR,
    OP_RDLIST,
    OP_RDDOT,
    OP_RDQUOTE,
    OP_RDQQUOTE,
    OP_RDQQUOTEVEC,
    OP_RDUNQUOTE,
    OP_RDUQTSP,
    OP_RDVEC,
    OP_VALUEPRINT,
    OP_P0LIST,
    OP_P1LIST,
    OP_PVEC
} opcode;

/* control macros for Eval_Cycle */
#define s_goto(a) do {                            \
    operator = (a);                               \
    goto dispatch; } while (0)

#define s_save(a, b, c)  (                        \
    dump = cons(envir, cons((c), dump)),          \
    dump = cons((b), dump),                       \
    dump = cons(mk_exact((long)(a)), dump))       \

#define s_return(a) do {                          \
    value = (a);                                  \
    operator = (unsigned short)ivalue(car(dump)); \
    args = cadr(dump);                            \
    envir = caddr(dump);                          \
    code = cadddr(dump);                          \
    dump = cddddr(dump);                          \
    goto dispatch; } while (0)

#define s_retbool(tf)    s_return((tf) ? T : F)

#define Error_0(s) do {                           \
    args = cons(mk_string((s)), NIL);             \
    operator = OP_ERR0;                           \
    goto dispatch; } while (0)

#define Error_1(s, a) do {                        \
    args = cons((a), NIL);                        \
    args = cons(mk_string((s)), args);            \
    operator = OP_ERR0;                           \
    goto dispatch; } while (0)

/* kernel of this intepreter */
static void Eval_Cycle(opcode operator)
{
    cell *x, *y, *value;
    long v, start, end, radix, width = 0;
    int index, i;
    int print_flag = 0;
    int token = 0;
    FILE *tmpfp = NULL;
    str str_x, str_y;

    dispatch: switch (operator) {
    case OP_LOAD: /* load */
        if (!isstring(car(args)))
            Error_0("load -- argument is not string");

        clearinput();

        if (is_interactive)
            fprintf(outfp, "loading %s...\n", strvalue(car(args)));

        in_filename = str_ref(string(car(args)));
        is_interactive = 0;
/*
        if ((infp = fopen(strvalue(car(args)), "r")) == NULL) {
            infp = stdin;
            is_interactive = 1;
            Error_1("Unable to open", car(args));
        } else {
            is_interactive = 0;
        }
*/
        s_goto(OP_T0LVL);

    case OP_T0LVL: /* top level */
        dump = NIL;
        envir = global_env;
        s_save(OP_VALUEPRINT, NIL, NIL);
        s_save(OP_T1LVL, NIL, NIL);

        if (is_interactive)
            fprintf(outfp, "\n");

        s_goto(OP_READ);

    case OP_T1LVL: /* top level */
        code = value;
        s_goto(OP_BEGIN);

    case OP_EVAL: /* main part of evalution */
        if (issymbol(code)) {    /* symbol */
            for (x = envir; x != NIL; x = cdr(x)) {
                for (y = car(x); y != NIL; y = cdr(y))
                    if (caar(y) == code)
                        break;
                if (y != NIL)
                    break;
            }
            if (x != NIL) {
                s_return(cdar(y));
            } else {
                Error_1("Unbounded variable:", code);
            }
        } else if (ispair(code)) {
            if (issyntax(x = car(code))) {    /* SYNTAX */
                code = cdr(code);
                s_goto(syntaxnum(x));
            } else {    /* first, eval top element and eval arguments */
                s_save(OP_E0ARGS, NIL, code);
                code = car(code);
                s_goto(OP_EVAL);
            }
        } else {
            s_return(code);
        }

    case OP_E0ARGS: /* eval arguments */
        if (ismacro(value)) {    /* macro expansion */
            s_save(OP_DOMACRO, NIL, NIL);
            args = cons(code, NIL);
            code = value;
            s_goto(OP_APPLY);
        } else {
            code = cdr(code);
            s_goto(OP_E1ARGS);
        }

    case OP_E1ARGS: /* eval arguments */
        args = cons(value, args);
        if (ispair(code)) {    /* continue */
            s_save(OP_E1ARGS, args, cdr(code));
            code = car(code);
            args = NIL;
            s_goto(OP_EVAL);
        } else {    /* end */
            args = reverse(args);
            code = car(args);
            args = cdr(args);
            s_goto(OP_APPLY);
        }

    case OP_APPLY: /* apply 'code' to 'args' */
        if (isproc(code)) {
            s_goto(procnum(code));    /* PROCEDURE */
        } else if (isclosure(code)) {    /* CLOSURE */
            /* make environment */
            envir = cons(NIL, closure_env(code));
            for (x = car(closure_code(code)), y = args; ispair(x); x = cdr(x), y = cdr(y)) {
                if (y == NIL) {
                    Error_1("Few arguments", x);
                } else {
                    car(envir) = cons(cons(car(x), car(y)), car(envir));
                }
            }
            if (x == NIL) {
                /*--
                 * if (y != NIL) {
                 *     Error_0("Many arguments");
                 * }
                 */
            } else if (issymbol(x))
                car(envir) = cons(cons(x, y), car(envir));
            else {
                Error_0("Syntax error in closure");
            }
            code = cdr(closure_code(code));
            args = NIL;
            s_goto(OP_BEGIN);
        } else if (iscontinuation(code)) {    /* CONTINUATION */
            dump = cont_dump(code);
            s_return(args != NIL ? car(args) : NIL);
        } else {
            Error_1("Illegal function:", code);
        }

    case OP_DOMACRO: /* do macro */
        code = value;
        s_goto(OP_EVAL);

    /****************************************************************************/

    case OP_QUOTE: /* quote */
        s_return(car(code));

    case OP_LAMBDA: /* lambda */
        s_return(mk_closure(code, envir));

    case OP_IF0: /* if */
        s_save(OP_IF1, NIL, cdr(code));
        code = car(code);
        s_goto(OP_EVAL);

    case OP_IF1: /* if */
        if (istrue(value))
            code = car(code);
        else
            code = cadr(code);    /* (if #f 1) ==> () because car(NIL) = NIL */
        s_goto(OP_EVAL);

    case OP_SET0: /* set! */
        s_save(OP_SET1, NIL, car(code));
        code = cadr(code);
        s_goto(OP_EVAL);

    case OP_SET1: /* set! */
        for (x = envir; x != NIL; x = cdr(x)) {
            for (y = car(x); y != NIL; y = cdr(y))
                if (caar(y) == code)
                    break;
            if (y != NIL)
                break;
        }
        if (x != NIL) {
            cdar(y) = value;
            s_return(value);
        } else {
            Error_1("Unbounded variable:", code);
        }

    case OP_BEGIN: /* begin */
        if (!ispair(code)) {
            s_return(code);
        }
        if (cdr(code) != NIL) {
            s_save(OP_BEGIN, NIL, cdr(code));
        }
        code = car(code);
        s_goto(OP_EVAL);

    case OP_COND0: /* cond */
        if (!ispair(code)) {
            Error_0("Syntax error in cond");
        }
        s_save(OP_COND1, NIL, code);
        code = caar(code);
        s_goto(OP_EVAL);

    case OP_COND1: /* cond */
        if (istrue(value)) {
            if ((code = cdar(code)) == NIL) {
                s_return(value);
            }

            if (!code || car(code) == FEED_TO) {
                if (!ispair(cdr(code))) {
                    Error_0("syntax error in cond");
                }

                x = cons(QUOTE, cons(value, NIL));

                code = cons(cadr(code), cons(x, NIL));

                s_goto(OP_EVAL);
            }

            s_goto(OP_BEGIN);
        } else {
            if ((code = cdr(code)) == NIL) {
                s_return(NIL);
            } else {
                s_save(OP_COND1, NIL, code);
                code = caar(code);
                s_goto(OP_EVAL);
            }
        }

    case OP_AND0: /* and */
        if (code == NIL) {
            s_return(T);
        }
        s_save(OP_AND1, NIL, cdr(code));
        code = car(code);
        s_goto(OP_EVAL);

    case OP_AND1: /* and */
        if (isfalse(value)) {
            s_return(value);
        } else if (code == NIL) {
            s_return(value);
        } else {
            s_save(OP_AND1, NIL, cdr(code));
            code = car(code);
            s_goto(OP_EVAL);
        }

    case OP_OR0: /* or */
        if (code == NIL) {
            s_return(F);
        }
        s_save(OP_OR1, NIL, cdr(code));
        code = car(code);
        s_goto(OP_EVAL);

    case OP_OR1: /* or */
        if (istrue(value)) {
            s_return(value);
        } else if (code == NIL) {
            s_return(value);
        } else {
            s_save(OP_OR1, NIL, cdr(code));
            code = car(code);
            s_goto(OP_EVAL);
        }

    case OP_CASE0: /* case */
        s_save(OP_CASE1, NIL, cdr(code));
        code = car(code);
        s_goto(OP_EVAL);

    case OP_CASE1: /* case */
        for (x = code; x != NIL; x = cdr(x)) {
            if (!ispair(caar(x)))
                break;
            for (y = caar(x); y != NIL; y = cdr(y))
                if (eqv(car(y), value))
                    break;
            if (y != NIL)
                break;
        }
        if (x != NIL) {
            if (ispair(caar(x))) {
                code = cdar(x);
                s_goto(OP_BEGIN);
            } else { /* else */
                s_save(OP_CASE2, NIL, cdar(x));
                code = caar(x);
                s_goto(OP_EVAL);
            }
        } else {
            s_return(NIL);
        }

    case OP_CASE2: /* case */
        if (istrue(value)) {
            s_goto(OP_BEGIN);
        } else {
            s_return(NIL);
        }

    case OP_LET0: /* let */
        args = NIL;
        value = code;
        code = issymbol(car(code)) ? cadr(code) : car(code);
        s_goto(OP_LET1);

    case OP_LET1: /* let (caluculate parameters) */
        args = cons(value, args);
        if (ispair(code)) {    /* continue */
            if (!ispair(car(code)) || !ispair(cdar(code))) {
                Error_1("Bad syntax of binding spec in let :", car(code));
            }
            s_save(OP_LET1, args, cdr(code));
            code = cadar(code);
            args = NIL;
            s_goto(OP_EVAL);
        } else {    /* end */
            args = reverse(args);
            code = car(args);
            args = cdr(args);
            s_goto(OP_LET2);
        }

    case OP_LET2: /* let */
        envir = cons(NIL, envir);
        for (x = issymbol(car(code)) ? cadr(code) : car(code), y = args;
             y != NIL; x = cdr(x), y = cdr(y))
            car(envir) = cons(cons(caar(x), car(y)), car(envir));
        if (issymbol(car(code))) {    /* named let */
            if (!ispair(cadr(code)))
                Error_1("Bad syntax of binding in let :", car(code));
            for (x = cadr(code), args = NIL; x != NIL; x = cdr(x))
                args = cons(caar(x), args);
            x = mk_closure(cons(reverse(args), cddr(code)), envir);
            car(envir) = cons(cons(car(code), x), car(envir));
            code = cddr(code);
            args = NIL;
        } else {
            code = cdr(code);
            args = NIL;
        }
        s_goto(OP_BEGIN);

    case OP_LET0AST: /* let* */
        if (car(code) == NIL) {
            envir = cons(NIL, envir);
            code = cdr(code);
            s_goto(OP_BEGIN);
        }
        if(!ispair(car(code)) || !ispair(caar(code)) || !ispair(cdaar(code))) {
            Error_1("Bad syntax of binding spec in let* :", car(code));
        }
        s_save(OP_LET1AST, cdr(code), car(code));
        code = cadaar(code);
        s_goto(OP_EVAL);

    case OP_LET1AST: /* let* (make new frame) */
        envir = cons(NIL, envir);
        s_goto(OP_LET2AST);

    case OP_LET2AST: /* let* (calculate parameters) */
        car(envir) = cons(cons(caar(code), value), car(envir));
        code = cdr(code);
        if (ispair(code)) {    /* continue */
            s_save(OP_LET2AST, args, code);
            code = cadar(code);
            args = NIL;
            s_goto(OP_EVAL);
        } else {    /* end */
            code = args;
            args = NIL;
            s_goto(OP_BEGIN);
        }

    case OP_LET0REC: /* letrec */
        envir = cons(NIL, envir);
        args = NIL;
        value = code;
        code = car(code);
        s_goto(OP_LET1REC);

    case OP_LET1REC: /* letrec (calculate parameters) */
        args = cons(value, args);
        if (ispair(code)) {    /* continue */
            if (!ispair(car(code)) || !ispair(cdar(code))) {
                Error_1("Bad syntax of binding spec in letrec :", car(code));
            }
            s_save(OP_LET1REC, args, cdr(code));
            code = cadar(code);
            args = NIL;
            s_goto(OP_EVAL);
        } else {    /* end */
            args = reverse(args);
            code = car(args);
            args = cdr(args);
            s_goto(OP_LET2REC);
        }

    case OP_LET2REC: /* letrec */
        for (x = car(code), y = args; y != NIL; x = cdr(x), y = cdr(y))
            car(envir) = cons(cons(caar(x), car(y)), car(envir));
        code = cdr(code);
        args = NIL;
        s_goto(OP_BEGIN);

    case OP_DELAY: /* delay */
        x = mk_closure(cons(NIL, code), envir);
        setpromise(x);
        s_return(x);

    case OP_DEF0: /* define */
        if (ispair(car(code))) {
            x = caar(code);
            code = cons(LAMBDA, cons(cdar(code), cdr(code)));
        } else {
            x = car(code);
            code = cadr(code);
        }
        if (!issymbol(x)) {
            Error_0("Variable is not symbol");
        }
        s_save(OP_DEF1, NIL, x);
        s_goto(OP_EVAL);

    case OP_DEF1: /* define */
        for (x = car(envir); x != NIL; x = cdr(x))
            if (caar(x) == code)
                break;
        if (x != NIL)
            cdar(x) = value;
        else
            car(envir) = cons(cons(code, value), car(envir));
        s_return(code);

    case OP_NOT: /* not */
        s_retbool(isfalse(car(args)));

    case OP_BOOL: /* boolean? */
        s_retbool(car(args) == F || car(args) == T);

    case OP_EQV: /* eqv? */
        s_retbool(eqv(car(args), cadr(args)));

    case OP_EQ: /* eq? */
        s_retbool(car(args) == cadr(args));

    case OP_EQUAL: /* equal? */
        s_retbool(equal(car(args), cadr(args)));

    case OP_PAIR: /* pair? */
        s_retbool(ispair(car(args)));

    case OP_CONS: /* cons */
        cdr(args) = cadr(args);
        s_return(args);

    case OP_CAR: /* car */
        if (ispair(car(args))) {
            s_return(caar(args));
        } else {
            Error_0("Unable to car for non-cons cell");
        }

    case OP_CDR: /* cdr */
        if (ispair(car(args))) {
            s_return(cdar(args));
        } else {
            Error_0("Unable to cdr for non-cons cell");
        }

    case OP_SETCAR: /* set-car! */
        if (ispair(car(args))) {
            caar(args) = cadr(args);
            s_return(car(args));
        } else {
            Error_0("Unable to set-car! for non-cons cell");
        }

    case OP_SETCDR: /* set-cdr! */
        if (ispair(car(args))) {
            cdar(args) = cadr(args);
            s_return(car(args));
        } else {
            Error_0("Unable to set-cdr! for non-cons cell");
        }

    case OP_NULL: /* null? */
        s_retbool(car(args) == NIL);

    case OP_LISTP: /* list? */
        s_retbool(list_length(car(args)) >= 0);

    case OP_LIST: /* list */
        s_return(args);

    case OP_LIST_LENGTH: /* length */
        v = list_length(car(args));
        if (v < 0) {
            Error_1("length: not a list:", car(args));
        }
        s_return(mk_exact(v));

    case OP_APPEND: /* append */
        x = NIL;
        y = args;
        if (y == x) {
            s_return(x);
        }

        /* cdr() in the while condition is not a typo. If car() */
        /* is used (append '() 'a) will return the wrong result.*/
        while (cdr(y) != NIL) {
            x = revappend(x, car(y));
            y = cdr(y);
            if (x == F) {
                Error_0("non-list argument to append");
            }
        }

        s_return(non_alloc_rev(car(y), x));

    case OP_REVERSE: /* reverse */
        s_return(reverse(car(args)));

    case OP_LIST_TAIL: /* list-tail */
        x = car(args);
        y = cadr(args);
        if (isexact(y) && (v = ivalue(y)) >= 0) {
            while(v > 0 && x != NIL && ispair(x)) {
                x = cdr(x);
                --v;
            }
            if (ispair(x))
                s_return(x);
        }
        s_return(NIL);

    case OP_LIST_REF: /* list-ref */
        x = car(args);
        y = cadr(args);
        if (isexact(y) && (v = ivalue(y)) >= 0) {
            while(v > 0 && x != NIL) {
                x = cdr(x);
                --v;
            }
            if (ispair(x))
                s_return(car(x));
        }
        s_return(NIL);

    case OP_MEMQ: /* memq */
        x = car(args);
        y = cadr(args);
        do {
            if (y == NIL)
                s_return(F);
            else if (x == car(y))
                s_return(y);
            y = cdr(y);
        } while (ispair(y));
        s_return(F);

    case OP_MEMV: /* memv */
        x = car(args);
        y = cadr(args);
        do {
            if (y == NIL)
                s_return(F);
            else if (eqv(x, car(y)))
                s_return(y);
            y = cdr(y);
        } while (ispair(y));
        s_return(F);

    case OP_MEMBER: /* member */
        x = car(args);
        y = cadr(args);
        do {
            if (y == NIL)
                s_return(F);
            else if (equal(x, car(y)))
                s_return(y);
            y = cdr(y);
        } while (ispair(y));
        s_return(F);

    case OP_ASSQ: /* assq */
        x = car(args);
        for (y = cadr(args); ispair(y); y = cdr(y)) {
            if (!ispair(car(y)))
                Error_0("Unable to handle non pair element");
            if (x == caar(y))
                break;
        }
        if (ispair(y)) {
            s_return(car(y));
        } else {
            s_return(F);
        }

    case OP_ASSV: /* assv */
        x = car(args);
        for (y = cadr(args); ispair(y); y = cdr(y)) {
            if (!ispair(car(y)))
                Error_0("Unable to handle non pair element");
            if (eqv(x, caar(y)))
                break;
        }
        if (ispair(y)) {
            s_return(car(y));
        } else {
            s_return(F);
        }

    case OP_ASSOC: /* assoc */
        x = car(args);
        for (y = cadr(args); ispair(y); y = cdr(y)) {
            if (!ispair(car(y)))
                Error_0("Unable to handle non pair element");
            if (equal(x, caar(y)))
                break;
        }
        if (ispair(y)) {
            s_return(car(y));
        } else {
            s_return(F);
        }

    case OP_SYMBOL: /* symbol? */
        s_retbool(issymbol(car(args)));

    case OP_SYMBOL_STRING: /* symbol->string */
        x = mk_string(symname(car(args)));
        s_return(x);

    case OP_STRING_SYMBOL:  /* string->symbol */
        s_return(mk_symbol(strvalue(car(args))));

    case OP_NUMBER:   /* number? */
    case OP_COMPLEX:  /* complex? */
    case OP_REAL:     /* real? */
    case OP_RATIONAL: /* rational? */
    case OP_INTEGER:  /* integer? */
        s_retbool(isnumber(car(args)));

    case OP_EXACT:   /* exact? */
        s_retbool(isexact(car(args)));

    case OP_INEXACT: /* inexact? */
        s_retbool(isinexact(car(args)));

    case OP_NEQ: /* = */
        x = car(args);
        for (y = cdr(args); y != NIL; y = cdr(y)) {
            if (isnumber(x) && isnumber(car(y)) && (num_rvalue(x) == num_rvalue(car(y))))
                x = car(y);
            else
                s_return(F);
        }
        s_return(T);

    case OP_LESS: /* < */
        x = car(args);
        for (y = cdr(args); y != NIL; y = cdr(y)) {
            if (isnumber(x) && isnumber(car(y)) && (num_rvalue(x) < num_rvalue(car(y))))
                x = car(y);
            else
                s_return(F);
        }
        s_return(T);

    case OP_GRE: /* > */
        x = car(args);
        for (y = cdr(args); y != NIL; y = cdr(y)) {
            if (isnumber(x) && isnumber(car(y)) && (num_rvalue(x) > num_rvalue(car(y))))
                x = car(y);
            else
                s_return(F);
        }
        s_return(T);

    case OP_LEQ: /* <= */
        x = car(args);
        y = cadr(args);
        if (isexact(x) && isexact(y))
            s_retbool(ivalue(x) <= ivalue(y));
        else if (isnumber(x) && isnumber(y))
            s_retbool(num_rvalue(x) <= num_rvalue(y));
        s_return(F);

    case OP_GEQ:        /* >= */
        x = car(args);
        y = cadr(args);
        if (isexact(x) && isexact(y))
            s_retbool(ivalue(x) >= ivalue(y));
        else if (isnumber(x) && isnumber(y))
            s_retbool(num_rvalue(x) >= num_rvalue(y));
        s_return(F);

    case OP_ZEROP: /* zero? */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'zero?'");
        if (isexact(x))
            s_retbool(ivalue(x) == 0);
        else if (isinexact(x))
            s_retbool(rvalue(x) == 0);
        s_return(F);

    case OP_POSP: /* positive? */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'positive?'");
        if (isexact(x))
            s_retbool(ivalue(x) > 0);
        else if (isinexact(x))
            s_retbool(rvalue(x) > 0);
        s_return(F);

    case OP_NEGP: /* negative? */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'negative?'");
        if (isexact(x))
            s_retbool(ivalue(x) < 0);
        else if (isinexact(x))
            s_retbool(rvalue(x) < 0);
        s_return(F);

    case OP_ODD: /* odd? */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'odd?'");
        if (isexact(x))
            s_retbool(ivalue(x) % 2 != 0);
        s_return(F);

    case OP_EVEN: /* even? */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'even?'");
        if (isexact(x))
            s_retbool((ivalue(x) == 0) || (ivalue(x) % 2 == 0));
        s_return(F);

    case OP_MAX: /* max */
        x = car(args);
        for (y = cdr(args); y != NIL; y = cdr(y)) {
            if (isexact(x) && isexact(car(y)))
                x = num_ivalue(x) > num_ivalue(car(y)) ? x : car(y);
            else if (isnumber(x) && isnumber(car(y)))
                x = num_rvalue(x) > num_rvalue(car(y)) ? x : car(y);
            else
                Error_0("max on non-number");
        }
        s_return(x);

    case OP_MIN: /* min */
        x = car(args);
        for (y = cdr(args); y != NIL; y = cdr(y)) {
            if (isexact(x) && isexact(car(y)))
                x = num_ivalue(x) < num_ivalue(car(y)) ? x : car(y);
            else if (isnumber(x) && isnumber(car(y)))
                x = num_rvalue(x) < num_rvalue(car(y)) ? x : car(y);
            else
                Error_0("min on non-number");
        }
        s_return(x);

    case OP_ADD: /* + */
        x = car(args);
        if (x == NIL)
            s_return(mk_exact(0));
        for (y = cdr(args); y != NIL; y = cdr(y)) {
            if (isexact(x) && isexact(car(y)))
                x = mk_exact(ivalue(x) + ivalue(car(y)));
            else if (isnumber(x) && isnumber(car(y)))
                x = mk_inexact(num_rvalue(x) + num_rvalue(car(y)));
            else
                Error_0("Mathematical operation on non-number");
        }
        s_return(x);

    case OP_SUB: /* - */
        x = car(args);
        if (x == NIL)
            Error_0("missing argument to function '-'");
        if (cdr(args) == NIL) {
            if (isexact(x))
                x = mk_exact(-ivalue(x));
            else
                x = mk_inexact(-rvalue(x));
        } else {
            for (y = cdr(args); y != NIL; y = cdr(y)) {
                if (isexact(x) && isexact(car(y)))
                    x = mk_exact(ivalue(x) - ivalue(car(y)));
                else if (isnumber(x) && isnumber(car(y)))
                    x = mk_inexact(num_rvalue(x) - num_rvalue(car(y)));
                else
                    Error_0("Mathematical operation on non-number");
            }
        }
        s_return(x);

    case OP_MUL: /* * */
        x = mk_exact(1);
        for (y = args; y != NIL; y = cdr(y)) {
            if (isexact(x) && isexact(car(y)))
                x = mk_exact(ivalue(x) * ivalue(car(y)));
            else if (isnumber(x) && isnumber(car(y)))
                x = mk_inexact(num_rvalue(x) * num_rvalue(car(y)));
            else
                Error_0("Mathematical operation on non-number");
        }
        s_return(x);

    case OP_DIV: /* / */ 
        x = car(args);
        if (cdr(args) == NIL)
            x = num_div(mk_exact(1), x);
        else
            for (y = cdr(args); y != NIL; y = cdr(y))
                x = num_div(x, car(y));
        s_return(x);

    case OP_ABS: /* abs */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'abs'");
        if (isexact(x))
            s_return(mk_exact(ivalue(x) < 0 ? -ivalue(x) : ivalue(x)));
        else if (isinexact(x))
            s_return(mk_inexact(rvalue(x) < 0 ? -rvalue(x) : rvalue(x)));
        s_return(NIL);

    case OP_QUOTIENT: /* quotient */ 
        if ((x = car(args)) == NIL || (y = cadr(args)) == NIL)
            Error_0("missing argument to function 'quotient'");
        if (isnumber(x) && isnumber(y))
            s_return(num_quo(x, y));
        Error_0("Unsupported quotient operation");

    case OP_REMAINDER: /* remainder */
        if ((x = car(args)) == NIL || (y = cadr(args)) == NIL)
            Error_0("missing argument to function 'remainder'");
        if (isnumber(x) && isnumber(y))
            s_return(num_rem(x, y));
        Error_0("Unsupported remainder operation");

    case OP_MODULO: /* modulo */
        if ((x = car(args)) == NIL || (y = cadr(args)) == NIL)
            Error_0("missing argument to function 'modulo'");
        y = cadr(args);
        if (isnumber(x) && isnumber(y))
            s_return(num_mod(x, y));
        Error_0("Unsupported remainder operation");

    case OP_FLOOR: /* floor */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'floor'");
        if (isnumber(x))
            s_return(mk_exact(floor(num_rvalue(x))));
        s_return(NIL);

    case OP_CEILING: /* ceiling */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'ceiling'");
        if (isnumber(x))
            s_return(mk_exact(ceil(num_rvalue(x))));
        s_return(NIL);

    case OP_TRUNCATE: /* truncate */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'truncate'");
        if (isnumber(x))
            s_return(mk_exact(trunc(num_rvalue(x))));
        s_return(NIL);

    case OP_ROUND: /* round */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'round'");
        if (isexact(x))
            s_return(x);
        else if (isnumber(x))
            s_return(mk_exact(round(num_rvalue(x))));
        s_return(NIL);

    case OP_EXP: /* exp */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'exp'");
        if (isnumber(x))
            s_return(mk_inexact(exp(num_rvalue(x))));
        s_return(NIL);

    case OP_LOG: /* log */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'log'");
        if (isnumber(x))
            s_return(mk_inexact(log(num_rvalue(x))));
        s_return(NIL);

    case OP_SIN: /* sin */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'sin'");
        if (isnumber(x))
            s_return(mk_inexact(sin(num_rvalue(x))));
        s_return(NIL);

    case OP_COS: /* cos */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'cos'");
        if (isnumber(x))
            s_return(mk_inexact(cos(num_rvalue(x))));
        s_return(NIL);

    case OP_TAN: /* tan */
        if ((x = car(args)) == NIL)
            Error_0("missing argument to function 'tan'");
        if (isnumber(x))
            s_return(mk_inexact(tan(num_rvalue(x))));
        s_return(NIL);

    case OP_ASIN: /* asin */
        x = car(args);
        if (isnumber(x))
            s_return(mk_inexact(asin(num_rvalue(x))));
        s_return(NIL);

    case OP_ACOS: /* acos */
        x = car(args);
        if (isnumber(x))
            s_return(mk_inexact(acos(num_rvalue(x))));
        s_return(NIL);

    case OP_ATAN: /* atan */
        x = car(args);
        y = cadr(args);
        if (isnumber(x) && y == NIL)
            s_return(mk_inexact(atan(num_rvalue(x))));
        else if (isnumber(x) && isnumber(y))
            s_return(mk_inexact(atan2(num_rvalue(x), num_rvalue(y))));
        s_return(NIL);

    case OP_SQRT: /* sqrt */
        x = car(args);
        if (isexact(x))
            s_return(mk_exact(sqrt(ivalue(x))));
        else if (isnumber(x))
            s_return(mk_inexact(sqrt(num_rvalue(x))));
        s_return(NIL);

    case OP_EXPT: /* expt */
        x = car(args);
        y = cadr(args);
        if (isexact(x) && isexact(x))
            s_return(mk_exact(pow(ivalue(x), ivalue(y))));
        else if (isnumber(x) && isnumber(x))
            s_return(mk_inexact(pow(num_rvalue(x), num_rvalue(y))));
        s_return(NIL);

    case OP_EXACT_INEXACT: /* exact->inexact */
        x = car(args);
        if (isnumber(x))
            s_return(mk_inexact(num_rvalue(x)));
        s_return(NIL);

    case OP_INEXACT_EXACT: /* inexact->exact */
        x = car(args);
        if (isnumber(x))
            s_return(mk_exact(num_ivalue(x)));

        Error_1("inexact->exact: not integral:", x);

    case OP_NUMBER_TO_STRING: /* number->string */
        x = car(args);
        y = cadr(args);
        radix = 0;

        if (!isnumber(x))
            Error_1("number->string not a number:", x);

        if (y == NIL) {
            radix = 10;
        } else if (isexact(y)) {
            v = ivalue(y);
            if (v == 2 || v == 8 || v == 10 || v == 16)
                radix = v;
        }

        if (radix == 0 || (isinexact(x) && radix != 10)) {
            Error_1("number->string invalid radix:", y);
        }

        if (isexact(x)) {
            s_return(mk_string_itoa(ivalue(x), radix));
        } else {
            sprintf(strbuf, "%lf", num_rvalue(x));
            s_return(mk_string(strbuf));
        }

    case OP_STRING_TO_NUMBER: /* string->number */
        x = car(args);
        y = cadr(args);
        radix = 0;

        if (isstring(x)) {
            char *end;
            const char *str = strvalue(x);

            if (*str == 0)
                s_return(F);

            if (y == NIL) {
                radix = 10;
            } else if (isexact(y)) {
                v = ivalue(y);
                if (v == 2 || v == 8 || v == 10 || v == 16)
                    radix = v;
            }

            if (radix == 0) {
                Error_1("string->number invalid radix:", y);
            }

            /* override provided radix if needed */
            if (*str == '#') {
                ++str;
                switch(*str) {
                    case 'b':
                        radix = 2;
                        break;
                    case 'o':
                        radix = 8;
                        break;
                    case 'd':
                        radix = 10;
                        break;
                    case 'x':
                        radix = 16;
                        break;
                    default:
                        s_return(F);
                }
                ++str;
            }

            v = strtol(str, &end, radix);

            if (*end == '\0') {
                s_return(mk_exact(v));
            } else if (radix == 10) {
                double d = strtod(str, &end);
                if (*end == '\0')
                    s_return(mk_inexact(d));
            }
        }
        s_return(F);

    case OP_CHARP: /* char? */
        s_retbool(ischar(car(args)));

    case OP_CHAR_EQ: /* char=? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char=? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char=? second argument not a char");

        s_retbool(cvalue(x) == cvalue(car(y)));

    case OP_CHAR_LT: /* char<? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char<? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char<? second argument not a char");

        s_retbool(cvalue(x) < cvalue(car(y)));

    case OP_CHAR_GT: /* char>? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char>? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char>? second argument not a char");

        s_retbool(cvalue(x) > cvalue(car(y)));

    case OP_CHAR_LE: /* char<=? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char<=? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char<=? second argument not a char");

        s_retbool(cvalue(x) <= cvalue(car(y)));

    case OP_CHAR_GE: /* char>=? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char>=? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char>=? second argument not a char");

        s_retbool(cvalue(x) >= cvalue(car(y)));

    case OP_CHAR_CI_EQ: /* char-ci=? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char-ci=? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char-ci=? second argument not a char");

        s_retbool(utf8_tolower(cvalue(x)) == utf8_tolower(cvalue(car(y))));

    case OP_CHAR_CI_LT: /* char-ci<? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char-ci<? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char-ci<? second argument not a char");

        s_retbool(utf8_tolower(cvalue(x)) < utf8_tolower(cvalue(car(y))));

    case OP_CHAR_CI_GT: /* char-ci>? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char-ci>? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char-ci>? second argument not a char");

        s_retbool(utf8_tolower(cvalue(x)) > utf8_tolower(cvalue(car(y))));

    case OP_CHAR_CI_LE: /* char-ci<=? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char-ci<=? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char-ci<=? second argument not a char");

        s_retbool(utf8_tolower(cvalue(x)) <= utf8_tolower(cvalue(car(y))));

    case OP_CHAR_CI_GE: /* char-ci>=? */
        x = car(args);
        y = cdr(args);

        if (!ischar(x))
            Error_0("char-ci>=? first argument not a char");
        else if (y == NIL || !ischar(car(y))) 
            Error_0("char-ci>=? second argument not a char");

        s_retbool(utf8_tolower(cvalue(x)) >= utf8_tolower(cvalue(car(y))));

    case OP_CHAR_ALPHAP: /* char-alphabetic? */
        x = car(args);

        if (!ischar(x))
            Error_0("char-alphabetic? argument not a char");

        s_retbool(utf8_isalpha(cvalue(x)));

    case OP_CHAR_NUMBERP: /* char-numeric? */
        x = car(args);

        if (!ischar(x))
            Error_0("char-numeric? argument not a char");

        s_retbool(utf8_isdigit(cvalue(x)));

    case OP_CHAR_WSP: /* char-whitespace? */
        x = car(args);

        if (!ischar(x))
            Error_0("char-whitespace? argument not a char");

        s_retbool(utf8_isspace(cvalue(x)));

    case OP_CHAR_UPPERP: /* char-upper-case? */
        x = car(args);

        if (!ischar(x))
            Error_0("char-upper-case? argument not a char");

        s_retbool(utf8_isupper(cvalue(x)));

    case OP_CHAR_LOWERP: /* char-lower-case? */
        x = car(args);

        if (!ischar(x))
            Error_0("char-lower-case? argument not a char");

        s_retbool(utf8_islower(cvalue(x)));

    case OP_CHAR_INT: /* char->integer */
        x = car(args);

        if (!ischar(x))
            Error_0("char->integer argument not a char");

        s_return(mk_exact(cvalue(x)));

    case OP_INT_CHAR: /* integer->char */
        x = car(args);

        if (!isexact(x))
            Error_0("integer->char argument not an integer");

        s_return(mk_character((char32_t)ivalue(x)));

    case OP_CHAR_UPCASE: /* char-upcase */
        x = car(args);

        if (!ischar(x))
            Error_0("char-upcase argument not a char");

        s_return(mk_character(utf8_toupper(cvalue(x))));

    case OP_CHAR_DOWNCASE: /* char-downcase */
        x = car(args);

        if (!ischar(x))
            Error_0("char-downcase argument not a char");

        s_return(mk_character(utf8_tolower(cvalue(x))));

    case OP_STRINGP: /* string? */
        s_retbool(isstring(car(args)));

    case OP_MAKE_STRING: /* make-string */
        x = car(args);
        y = cdr(args);

        if (isnumber(x))
            v = num_ivalue(x);
        else
            Error_0("make-string first argument not a number");

        if (y != NIL) {
            if (ischar(car(y)))
                s_return(mk_empty_string(v, cvalue(car(y))));
            else
                Error_0("make-string second argument not a character");
        } else {
            s_return(mk_empty_string(v, ' '));
        }

    case OP_STRING: /* string */
        i = 0;
        y = args;

        while (y != NIL) {
            if (!ischar(car(y))) {
                Error_1("list->string argument not a character: ", car(y));
            }

            i += u8_wc_toutf8(strbuf+i, cvalue(car(y)));

            y = cdr(y);
        }

        strbuf[i] = '\0';
        s_return(mk_string(strbuf));

    case OP_STRING_LENGTH: /* string-length */
        x = car(args);
        if (isstring(x)) {
            s_return(mk_exact(u8_strlen(strvalue(x))));
        }

        Error_0("string-length argument not a string");

    case OP_STRING_REF: /* string-ref */
        x = car(args);
        y = cadr(args);
        if (!isstring(x)) {
            Error_0("string-ref argument not a string");
        } else if (isexact(y)) {
            v = ivalue(y);

            if (v >= 0 && v < u8_strlen(strvalue(x))) {
                i = 0;
                uint32_t c = u8_nextchar(strvalue(x), &i);

                for (index = 0; index <= v; index++) {
                    c = u8_nextchar(strvalue(x), &i);
                }

                s_return(mk_character(c));
            }
        }
        Error_1("Bad index into string :", y);

    case OP_STRING_SET: /* string-set! */
        x = car(args);
        y = cadr(args);

        if (!isstring(x)) {
            Error_0("string-set! argument not a string");
        } else if (!isexact(y)) {
            Error_1("string-set! argument not an index value :", y);
        } else if (!ischar(caddr(args))) {
            Error_1("string-set! argument not a character :", caddr(args));
        }

        index = ivalue(y);

        if (index >= 0 && index < u8_strlen(strvalue(x))) {
            uint32_t c = cvalue(caddr(args));
            i = 0;

            i = u8_offset(strvalue(x), index);

            /* copy string up to index */
            strncpy(strbuf, strvalue(x), i);

            /* append replacement character */
            start = i + u8_wc_toutf8(strbuf+i, c);
            end = str_len(string(x))-i;

            /* advance string past replaced character */
            u8_inc(strvalue(x), &i);

            /* copy rest of string */
            strncpy(strbuf+start, strvalue(x)+i, end);
            strbuf[start+end] = '\0';

            str_cpy(&string(x), str_ref(strbuf));
            s_return(x);
        }

        Error_1("Bad index into string :", y);

    case OP_STRING_EQ: /* string=? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string=? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string=? argument not a string: ", car(y));
            } else if (!str_eq(string(x), string(car(y)))) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_CI_EQ: /* string-ci=? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string-ci=? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string-ci=? argument not a string: ", car(y));
            } else if (!str_eq_ci(string(x), string(car(y)))) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_LT: /* string<? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string<? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string<? argument not a string: ", car(y));
            } else if (str_cmp(string(x), string(car(y))) >= 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_CI_LT: /* string-ci<? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string-ci<? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string-ci<? argument not a string: ", car(y));
            } else if (str_cmp(string(x), string(car(y))) >= 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_GT: /* string>? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string>? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string>? argument not a string: ", car(y));
            } else if (str_cmp(string(x), string(car(y))) <= 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_CI_GT: /* string-ci>? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string-ci>? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string-ci>? argument not a string: ", car(y));
            } else if (str_cmp_ci(string(x), string(car(y))) <= 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_LE: /* string<=? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string<=? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string<=? argument not a string: ", car(y));
            } else if (str_cmp(string(x), string(car(y))) > 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_CI_LE: /* string-ci<=? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string-ci<=? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string-ci<=? argument not a string: ", car(y));
            } else if (str_cmp(string(x), string(car(y))) > 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_GE: /* string>=? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string>=? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string>=? argument not a string: ", car(y));
            } else if (str_cmp(string(x), string(car(y))) < 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_STRING_CI_GE: /* string-ci>=? */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string-ci>=? argument not a string: ", x);

        while (y != NIL) {
            if (!isstring(car(y))) {
                Error_1("string-ci>=? argument not a string: ", car(y));
            } else if (str_cmp_ci(string(x), string(car(y))) < 0) {
                s_retbool(0);
            }
            x = car(y);
            y = cdr(y);
        }

        s_retbool(1);

    case OP_SUBSTRING: /* substring */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("substring argument not a string: ", x);

        if (!isnumber(car(y)) || num_ivalue(car(y)) < 0 || num_ivalue(car(y)) > u8_strlen(strvalue(x))) {
            Error_1("substring 'start' argument not a valid index: ", car(y));
        }

        start = num_ivalue(car(y));
        y = cdr(y);

        if (!isnumber(car(y)) || num_ivalue(car(y)) < start || num_ivalue(car(y)) > u8_strlen(strvalue(x))) {
            Error_1("substring 'end' argument not a valid index: ", car(y));
        }

        end = num_ivalue(car(y));

        i = 0;
        for (index = 0; index < start; index++) {
            u8_inc(strvalue(x), &i);
        }

        for (index = 0; index < end - start; index++) {
            uint32_t c = u8_nextchar((char *)strvalue(x), &i);
            u8_toutf8(strbuf+index, LINESIZE, &c, 1);
        }

        s_return(mk_string(strbuf));

    case OP_STRING_APPEND: /* string-append */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string-append argument not a string: ", x);

        str_cpy(&str_x, string(x));

        while (y != NIL) {
            if (!isstring(car(y))) {
                str_clear(&str_x);
                Error_1("string-append argument not a string: ", car(y));
            }

            str_swap(&str_x, &str_y);

            str_cat(&str_x, str_y, string(car(y)));
            y = cdr(y);
        }

        x = mk_string(str_ptr(str_x));
        str_clear(&str_x);
        str_clear(&str_y);
        s_return(x);

    case OP_STRING_LIST: /* string->list */
        x = car(args);
        y = cdr(args);

        if (!isstring(x))
            Error_1("string->list argument not a string: ", x);

        str_x = str_ref(string(x));
        v = u8_strlen(str_ptr(str_x));
        x = NIL;

        if (y == NIL) {
            start = 0;
            end = v;
        } else if (isnumber(car(y))) {
           start = num_ivalue(car(y));
           if (start < 0 || start > v) {
               Error_1("string->list 'start' argument not a valid index: ", car(y));
           }
           y = cdr(y);
        } else {
            Error_1("string->list 'start' argument not a valid index: ", car(y));
        }

        if (y == NIL) {
            end = v;
        } else if (isnumber(car(y))) {
           end = num_ivalue(car(y));
           end = (end >= v ? v-1 : end);
           if (end < start) {
               s_return(NIL);
           }
        } else {
            Error_1("string->list 'start' argument not a valid index: ", car(y));
        }

        i = 0;

        for (index = 0; index < start; index++) {
            u8_inc(str_ptr(str_x), &i);
        }

        for (index = 0; index < end; index++) {
            uint32_t c = u8_nextchar((char *)str_ptr(str_x), &i);
            if (x == NIL)
                x = cons(mk_character(c), NIL);
            else
                list_append(x, cons(mk_character(c), NIL));
        }

        s_return(x);

    case OP_LIST_STRING: /* list->string */
        i = 0;
        y = car(args);

        while (y != NIL) {
            if (!ischar(car(y))) {
                Error_1("list->string argument not a character: ", car(y));
            }

            i += u8_wc_toutf8(strbuf+i, cvalue(car(y)));

            y = cdr(y);
        }

        strbuf[i] = '\0';
        s_return(mk_string(strbuf));

    case OP_STRING_COPY: /* string-copy */
        x = car(args);
        y = cdr(args);

        str_x = str_ref(string(x));
        v = u8_strlen(str_ptr(str_x));

        if (!isstring(x))
            Error_1("string-copy argument not a string: ", x);

        if (y == NIL) {
            start = 0;
            end = v;
        } else if (isnumber(car(y))) {
            start = num_ivalue(car(y));
            if (start < 0 || start > v) {
                Error_1("string->copy 'start' argument not a valid index: ", car(y));
            }
            y = cdr(y);
        } else {
            Error_1("string->copy 'start' argument not a valid index: ", car(y));
        }

        if (y == NIL) {
            end = v;
        } else if (isnumber(car(y))) {
            end = num_ivalue(car(y));
            end = (end >= v ? v-1 : end);
            if (end < start) {
                s_return(NIL);
            }
        } else {
            Error_1("string->copy 'start' argument not a valid index: ", car(y));
        }

        i = 0;
        for (index = 0; index < start; index++) {
            u8_inc(strvalue(x), &i);
        }

        for (index = 0; index < end - start; index++) {
            uint32_t c = u8_nextchar((char *)strvalue(x), &i);
            u8_toutf8(strbuf+index, LINESIZE, &c, 1);
        }

        s_return(mk_string(strbuf));

    case OP_STRING_FILL: /* string-fill! */
        x = car(args);
        y = cdr(args);

        if (!isstring(x)) {
            Error_1("string-fill! argument not a string: ", x);
        } else if (!ischar(car(y))) {
            Error_1("string-fill! argument not a character :", y);
        }

        str_x = str_ref(string(x));
        v = u8_strlen(str_ptr(str_x));

        s_return(mk_empty_string(v, cvalue(car(y))));

    case OP_STRING_NULL: /* string-null? */
        x = car(args);

        if (!isstring(x))
            Error_0("string-null? argument not a string");
        else
            s_retbool(str_is_empty(string(x)));

#if 0
    case OP_STRING_JOIN: /* string-join */
        x = car(args);
        y = cdr(args);
        s_return(F);
#endif

    case OP_VECTORP: /* vector? */
        s_retbool(isvector(car(args)));

    case OP_MAKE_VECTOR: /* make-vector */
        x = car(args);
        if (isnumber(x))
            v = num_ivalue(x);
        else
            Error_1("make-vector first argument not a number :", x);
        s_return(mk_vector(v, cadr(args)));

    case OP_VECTOR: /* vector */
        v = list_length(args);
        if (v < 0)
            Error_1("vector: not a proper list:", args);
        s_return(mk_vector(v, args));

    case OP_VECTOR_LENGTH: /* vector-length */
        x = car(args);
        if (isvector(x))
            s_return(mk_exact(ivalue(x)));
        Error_1("vector-length argument not a vector", x);

    case OP_VECTOR_REF: /* vector-ref */
        x = car(args);
        y = cadr(args);
        if (isvector(x) && isexact(y)) {
            v = ivalue(y);
            if (v >= 0 && v < ivalue(x))
                s_return(vector_elem(x, v));
        }
        Error_1("Bad index into vector :", y);

    case OP_VECTOR_SET: /* vector-set! */
        x = car(args);
        y = cadr(args);
        if (isvector(x) && isexact(y)) {
            v = ivalue(y);
            if (v >= 0 && v < ivalue(x)) {
                set_vector_elem(x, v, caddr(args));
                s_return(x);
            }
        }
        Error_1("Bad index into vector :", y);

    case OP_VECTOR_LIST: /* vector->list */
        x = car(args);
        y = NIL;
        if (!isvector(x)) {
            Error_1("vector->list argument not a vector :", x);
        } else if (ivalue(x) != 0) {
            y = cons(vector_elem(x, 0), NIL);
            for (v = 1; v < ivalue(x); v++)
                tail_append(y, vector_elem(x, v));
        }
        s_return(y);

    case OP_LIST_VECTOR: /* list->vector */
        args = car(args);
        s_goto(OP_VECTOR);

    case OP_VECTOR_FILL: /* vector-fill! */
        x = car(args);
        y = cadr(args);
        if (isvector(x)) {
            fill_vector(x, y);
            s_return(x);
        }
        Error_1("vector-fill! argument not a vector :", x);

    case OP_PROC: /* procedure? */
        /*--
         * continuation should be procedure by the example
         * (call-with-current-continuation procedure?) ==> #t
         * in R^3 report sec. 6.9
         */
        x = car(args);
        s_retbool(isproc(x) || isclosure(x) || iscontinuation(x));

    case OP_PAPPLY: /* apply */
        code = car(args);
        args = list_star(cdr(args));
        s_goto(OP_APPLY);

    case OP_FORCE: /* force */
        code = car(args);
        if (ispromise(code)) {
            s_save(OP_SAVE_FORCED, NIL, code);
            args = NIL;
            s_goto(OP_APPLY);
        } else {
            s_return(code);
        }

    case OP_SAVE_FORCED: /* Save forced value replacing promise */
        memcpy(code, value, sizeof(cell));
        s_return(value);

    case OP_CONTINUATION: /* call-with-current-continuation */
        code = car(args);
        args = cons(mk_continuation(dump), NIL);
        s_goto(OP_APPLY);

    case OP_INPUTP: /* input-port? */
        x = car(args);
        s_retbool(isport(x) && isinput(x));

    case OP_OUTPUTP: /* output-port? */
        x = car(args);
        s_retbool(isport(x) && !isinput(x));

    case OP_CURRENT_IN_PORT: /* current-input-port */
        s_return(mk_port(infp, 1));

    case OP_CURRENT_OUT_PORT: /* current-output-port */
        s_return(mk_port(outfp, 0));
#if 0
    case OP_OPEN_INFILE: /* open-input-file */
        if (isstring(car(args)) && (tmpfp = fopen(strvalue(car(args)), "r")) != NULL)
            s_return(mk_port(tmpfp, 1));
        Error_1("unable to open file", car(args));

    case OP_OPEN_OUTFILE: /* open-output-file */
        if (isstring(car(args)) && (tmpfp = fopen(strvalue(car(args)), "w")) != NULL)
            s_return(mk_port(tmpfp, 0));
        Error_1("unable to open file", car(args));
#endif
    case OP_READ: /* read */
        for (token = lex(&x); token != 0; token = lex(&x)) {
            Parse(parser, token, x, NULL);
        }

        Parse(parser, 0, NULL, &value);

        s_return(value);

    /* TODO: write, display, newline and write-char take a second 'port' argument */
    case OP_WRITE: /* write */
        print_flag = 1;
        args = car(args);
        s_goto(OP_P0LIST);

    case OP_DISPLAY: /* display */
        print_flag = 0;
        args = car(args);
        s_goto(OP_P0LIST);

    case OP_NEWLINE: /* newline */
        fprintf(outfp, "\n");
        s_return(T);

    case OP_WRITE_CHAR: /* write-char */
        if (ischar(car(args))) {
            print_flag = 0;
            args = car(args);
            s_goto(OP_P0LIST);
        }
        Error_1("Argument to write-char not a char :", car(args));

    case OP_LOCALE_SET: /* locale-set! */
        x = car(args);
        y = cadr(args);

        if (isexact(x) && isstring(y)) {
            char *c = NULL;

            switch (ivalue(x)) {
                case LC_ALL:
                    c = setlocale(LC_ALL, strvalue(y));
                    break;
                case LC_COLLATE:
                    c = setlocale(LC_COLLATE, strvalue(y));
                    break;
                case LC_CTYPE:
                    c = setlocale(LC_CTYPE, strvalue(y));
                    break;
                case LC_MESSAGES:
                    c = setlocale(LC_MESSAGES, strvalue(y));
                    break;
                case LC_MONETARY:
                    c = setlocale(LC_MONETARY, strvalue(y));
                    break;
                case LC_NUMERIC:
                    c = setlocale(LC_NUMERIC, strvalue(y));
                    break;
                case LC_TIME:
                    c = setlocale(LC_TIME, strvalue(y));
                    break;
                default:
                    Error_1("locale-set! invalid locale category :", x);
            }

            if (c == NULL) {
                Error_1("locale-set! unable to set locale :", y);
            } else {
                s_return(mk_string(c));
            }
        }

        Error_0("locale-set! arguments not a locale category and a locale name");

    case OP_CLOCK: /* clock */
        s_return(mk_exact(clock()));

    case OP_TIME: /* time */
        s_return(mk_exact(time(NULL)));

    case OP_TIME_STRING: /* time->string */
        x = car(args);
        y = cadr(args);

        if (isexact(x) && isstring(y)) {
            if (strftime(strbuf, LINESIZE, strvalue(y), localtime(&ivalue(x))) == 0) {
                Error_0("time->string failed");
            }
            s_return(mk_string(strbuf));
        }

        Error_0("time->string arguments not an exact number and a format string");

    case OP_TIME_GMT: /* time->gmt */
        if (isexact(car(args))) {
            v = ivalue(car(args));
            s_return(mk_exact(mktime(gmtime(&v))));
        }
        Error_1("Argument to time->gmt not an exact number :", car(args));

    case OP_SEED: /* seed */
        x = car(args);

        if (isnumber(x)) {
            srand(num_ivalue(x));
        } else {
            Error_1("seed argument not a number", x);
        }
        s_return(T);

    case OP_ROLL: /* roll */
        x = car(args);
        y = cadr(args);

        if (isnumber(x)) {
            start = num_ivalue(x);
            if (isnumber(y)) {
                end = num_ivalue(y);
                s_return(mk_exact((rand() % (end - start + 1)) + start));
            } else if (y == NIL) {
                s_return(mk_exact(rand() % start + 1));
            }
        } else if (x == NIL) {
            s_return(mk_exact(rand()));
        }

        Error_0("roll arguments not numbers");

    case OP_PEVAL: /* eval */
        code = car(args);
        args = NIL;
        s_goto(OP_EVAL);

    case OP_ERR0: /* error */
        if (!isstring(car(args))) {
            Error_0("error -- first argument must be string");
        }
        tmpfp = outfp;
        outfp = stderr;
        fprintf(outfp, "Error: ");
        fprintf(outfp, "%s", strvalue(car(args)));
        args = cdr(args);
        s_goto(OP_ERR1);

    case OP_ERR1: /* error */
        fprintf(outfp, " ");
        if (args != NIL) {
            s_save(OP_ERR1, cdr(args), NIL);
            args = car(args);
            print_flag = 1;
            s_goto(OP_P0LIST);
        } else {
            fprintf(outfp, "\n");
            flushinput();
            outfp = tmpfp;
            s_goto(OP_T0LVL);
        }

    case OP_GENSYM: /* gensym */
        s_return(mk_gensym());

    case OP_QUIT: /* quit */
        return;

    case OP_GC: /* gc */
        gc(NIL, NIL);
        s_return(T);

    case OP_GCVERB: /* gc-verbose */
        v = gc_verbose;
        
        gc_verbose = (car(args) != F);
        s_retbool(v);

    case OP_NEWSEGMENT: /* new-segment */
        if (!isexact(car(args))) {
            Error_0("new-segment -- argument must be an exact number");
        }
        fprintf(outfp, "allocate %d new segments\n", alloc_cellseg((int) ivalue(car(args))));
        s_return(T);

    case OP_PUT: /* put */
        if (!hasprop(car(args)) || !hasprop(cadr(args))) {
            Error_0("Illegal use of put");
        }
        for (x = symprop(car(args)), y = cadr(args); x != NIL; x = cdr(x))
            if (caar(x) == y)
                break;
        if (x != NIL)
            cdar(x) = caddr(args);
        else
            symprop(car(args)) = cons(cons(y, caddr(args)), symprop(car(args)));
        s_return(T);

    case OP_GET: /* get */
        if (!hasprop(car(args)) || !hasprop(cadr(args))) {
            Error_0("Illegal use of get");
        }
        for (x = symprop(car(args)), y = cadr(args); x != NIL; x = cdr(x))
            if (caar(x) == y)
                break;
        if (x != NIL) {
            s_return(cdar(x));
        } else {
            s_return(NIL);
        }

    case OP_PRINT_WIDTH: /* print-width */    /* a.k */
        width = 0;
        args = car(args);
        print_flag = -1;
        s_goto(OP_P0_WIDTH);
        
    case OP_P0_WIDTH:
        if (!ispair(args)) {
            width += printatom(args, print_flag);
            s_return(mk_exact(width));
        } else if (car(args) == QUOTE && ok_abbrev(cdr(args))) {
            ++width;
            args = cadr(args);
            s_goto(OP_P0_WIDTH);
        } else if (car(args) == QQUOTE && ok_abbrev(cdr(args))) {
            ++width;
            args = cadr(args);
            s_goto(OP_P0_WIDTH);
        } else if (car(args) == UNQUOTE && ok_abbrev(cdr(args))) {
            ++width;
            args = cadr(args);
            s_goto(OP_P0_WIDTH);
        } else if (car(args) == UNQUOTESP && ok_abbrev(cdr(args))) {
            width += 2;
            args = cadr(args);
            s_goto(OP_P0_WIDTH);
        } else {
            ++width;
            s_save(OP_P1_WIDTH, cdr(args), NIL);
            args = car(args);
            s_goto(OP_P0_WIDTH);
        }
        
    case OP_P1_WIDTH:
        if (ispair(args)) {
            s_save(OP_P1_WIDTH, cdr(args), NIL);
            ++width;
            args = car(args);
            s_goto(OP_P0_WIDTH);
        } else {
            if (args != NIL)
                width += 3 + printatom(args, print_flag);
            ++width;
            s_return(mk_exact(width));
        }
        
    case OP_GET_CLOSURE: /* get-closure-code */
        args = car(args);
        if (args == NIL) {
            s_return(F);
        } else if (isclosure(args)) {
            s_return(cons(LAMBDA, closure_code(value)));
        } else if (ismacro(args)) {
            s_return(cons(LAMBDA, closure_code(value)));
        } else {
            s_return(F);
        }

    case OP_CLOSUREP: /* closure? */
        /*
         * Note, macro object is also a closure.
         * Therefore, (closure? <#MACRO>) ==> #t
         */
        if (car(args) == NIL) {
            s_return(F);
        }
        s_retbool(isclosure(car(args)));

    case OP_MACROP: /* macro? */
        if (car(args) == NIL) {
            s_return(F);
        }
        s_retbool(ismacro(car(args)));

    case OP_0MACRO:    /* macro */
        x = car(code);
        code = cadr(code);
        if (!issymbol(x)) {
            Error_1("Variable is not symbol", x);
        }
        s_save(OP_1MACRO, NIL, x);
        s_goto(OP_EVAL);

    case OP_1MACRO:    /* macro */
        type(value) |= T_MACRO;
        for (x = car(envir); x != NIL; x = cdr(x))
            if (caar(x) == code)
                break;
        if (x != NIL)
            cdar(x) = value;
        else
            car(envir) = cons(cons(code, value), car(envir));
        s_return(code);

    /* ========== printing part ========== */
    case OP_VALUEPRINT:    /* print evalution result */
        print_flag = 1;
        args = value;

        if (is_interactive) {
            s_save(OP_T0LVL, NIL, NIL);
            s_goto(OP_P0LIST);
        } else {
            s_goto(OP_T0LVL);
        }

    case OP_P0LIST:
        if (isvector(args)) {
            fprintf(outfp, "#(");
            args = cons(args, mk_exact(0));
            s_goto(OP_PVEC);
        } else if (!ispair(args)) {
            printatom(args, print_flag);
            s_return(T);
        } else if (car(args) == QUOTE && ok_abbrev(cdr(args))) {
            fprintf(outfp, "'");
            args = cadr(args);
            s_goto(OP_P0LIST);
        } else if (car(args) == QQUOTE && ok_abbrev(cdr(args))) {
            fprintf(outfp, "`");
            args = cadr(args);
            s_goto(OP_P0LIST);
        } else if (car(args) == UNQUOTE && ok_abbrev(cdr(args))) {
            fprintf(outfp, ",");
            args = cadr(args);
            s_goto(OP_P0LIST);
        } else if (car(args) == UNQUOTESP && ok_abbrev(cdr(args))) {
            fprintf(outfp, ",@");
            args = cadr(args);
            s_goto(OP_P0LIST);
        } else {
            fprintf(outfp, "(");
            s_save(OP_P1LIST, cdr(args), NIL);
            args = car(args);
            s_goto(OP_P0LIST);
        }

    case OP_P1LIST:
        if (ispair(args)) {
            s_save(OP_P1LIST, cdr(args), NIL);
            fprintf(outfp, " ");
            args = car(args);
            s_goto(OP_P0LIST);

        } else if (isvector(args)) {
            s_save(OP_P1LIST, NIL, NIL);
            fprintf(outfp, " . ");
            s_goto(OP_P0LIST);
        } else {
            if (args != NIL) {
                fprintf(outfp, " . ");
                printatom(args, print_flag);
            }
            fprintf(outfp, ")");
            s_return(T);
        }

    case OP_PVEC:
        x = car(args);
        v = ivalue(cdr(args));

        if (v == ivalue(x)) {
            fprintf(outfp, ")");
            s_return(T);
        } else {
            y = vector_elem(x, v);
            ivalue(cdr(args)) = v + 1;
            s_save(OP_PVEC, args, NIL);
            args = y;
            if (v > 0)
                fprintf(outfp, " ");
            s_goto(OP_P0LIST);
        }

    default:
        sprintf(strbuf, "%d is illegal operator", operator);
        Error_0(strbuf);
    }
}

/* ========== Error ==========  */

void error(const char *msg)
{
    fprintf(stderr, "Error: %s \n", msg);
    flushinput();
    longjmp(error_jmp, OP_T0LVL);
}

void fatal_error(const char *msg)
{
    fprintf(stderr, "Fatal error: %s \n", msg);
    exit(1);
}

/* ========== Initialization of internal keywords ========== */

static void init_vars_global()
{
    /* init input/output file */
    infp = stdin;
    outfp = stdout;
    is_interactive = 0;
    in_filename = str_null;

    /* init NIL */
    type(NIL) = (T_ATOM | MARK);
    car(NIL) = cdr(NIL) = NIL;

    /* init T */
    type(T) = (T_ATOM | MARK);
    car(T) = cdr(T) = T;

    /* init F */
    type(F) = (T_ATOM | MARK);
    car(F) = cdr(F) = F;

    /* init global_env */
    global_env = cons(NIL, NIL);

    /* init locale categories */
    car(global_env) = cons(cons(mk_symbol("LC_ALL"),      mk_exact(LC_ALL)),      car(global_env));
    car(global_env) = cons(cons(mk_symbol("LC_COLLATE"),  mk_exact(LC_COLLATE)),  car(global_env));
    car(global_env) = cons(cons(mk_symbol("LC_CTYPE"),    mk_exact(LC_CTYPE)),    car(global_env));
    car(global_env) = cons(cons(mk_symbol("LC_MESSAGES"), mk_exact(LC_MESSAGES)), car(global_env));
    car(global_env) = cons(cons(mk_symbol("LC_MONETARY"), mk_exact(LC_MONETARY)), car(global_env));
    car(global_env) = cons(cons(mk_symbol("LC_NUMERIC"),  mk_exact(LC_NUMERIC)),  car(global_env));
    car(global_env) = cons(cons(mk_symbol("LC_TIME"),     mk_exact(LC_TIME)),     car(global_env));

    /* init else */
    car(global_env) = cons(cons(mk_symbol("else"), T), car(global_env));

    parser = ParseAlloc(malloc);
}

/**** Expressions & Definitions ****/
static void init_syntax()
{
    mk_syntax(OP_QUOTE,            "quote");
    mk_syntax(OP_LAMBDA,           "lambda");
    mk_syntax(OP_IF0,              "if");
    mk_syntax(OP_SET0,             "set!");
    mk_syntax(OP_BEGIN,            "begin");
    mk_syntax(OP_COND0,            "cond");
    mk_syntax(OP_AND0,             "and");
    mk_syntax(OP_OR0,              "or");
    mk_syntax(OP_CASE0,            "case");
    mk_syntax(OP_LET0,             "let");
    mk_syntax(OP_LET0AST,          "let*");
    mk_syntax(OP_LET0REC,          "letrec");
    // mk_syntax(OP_DO,               "do");  // macro in init.scm
    mk_syntax(OP_DELAY,            "delay");
    mk_syntax(OP_DEF0,             "define");
    mk_syntax(OP_0MACRO,           "macro"); // not in r4rs
}

/**** 6. Standard procedures ****/
static void init_procs()
{
    /**** 6.1. Booleans ****/
    mk_proc(OP_NOT,                "not");
    mk_proc(OP_BOOL,               "boolean?");

    /**** 6.2. Equivalence predicates ****/
    mk_proc(OP_EQV,                "eqv?");
    mk_proc(OP_EQ,                 "eq?");
    mk_proc(OP_EQUAL,              "equal?");

    /**** 6.3. Pairs and lists ****/
    mk_proc(OP_PAIR,               "pair?");
    mk_proc(OP_CONS,               "cons");
    mk_proc(OP_CAR,                "car");
    mk_proc(OP_CDR,                "cdr");
    mk_proc(OP_SETCAR,             "set-car!");
    mk_proc(OP_SETCDR,             "set-cdr!");
    mk_proc(OP_NULL,               "null?");
    mk_proc(OP_LISTP,              "list?");
    mk_proc(OP_LIST,               "list");
    mk_proc(OP_LIST_LENGTH,        "length");
    mk_proc(OP_APPEND,             "append");
    mk_proc(OP_REVERSE,            "reverse");
    mk_proc(OP_LIST_TAIL,          "list-tail");
    mk_proc(OP_LIST_REF,           "list-ref");
    mk_proc(OP_MEMQ,               "memq");
    mk_proc(OP_MEMV,               "memv");
    mk_proc(OP_MEMBER,             "member");
    mk_proc(OP_ASSQ,               "assq");
    mk_proc(OP_ASSV,               "assv");
    mk_proc(OP_ASSOC,              "assoc");

    /**** 6.4. Symbols ****/
    mk_proc(OP_SYMBOL,             "symbol?");
    mk_proc(OP_SYMBOL_STRING,      "symbol->string");
    mk_proc(OP_STRING_SYMBOL,      "string->symbol");

    /**** 6.5.5. Numerical operations ****/
    mk_proc(OP_NUMBER,             "number?");
    mk_proc(OP_COMPLEX,            "complex?");
    mk_proc(OP_REAL,               "real?");
    mk_proc(OP_RATIONAL,           "rational?");
    mk_proc(OP_INTEGER,            "integer?");
    mk_proc(OP_EXACT,              "exact?");
    mk_proc(OP_INEXACT,            "inexact?");
    mk_proc(OP_NEQ,                "=");
    mk_proc(OP_LESS,               "<");
    mk_proc(OP_GRE,                ">");
    mk_proc(OP_LEQ,                "<=");
    mk_proc(OP_GEQ,                ">=");
    mk_proc(OP_ZEROP,              "zero?");
    mk_proc(OP_POSP,               "positive?");
    mk_proc(OP_NEGP,               "negative?");
    mk_proc(OP_ODD,                "odd?");
    mk_proc(OP_EVEN,               "even?");
    mk_proc(OP_MAX,                "max");
    mk_proc(OP_MIN,                "min");
    mk_proc(OP_ADD,                "+");
    mk_proc(OP_SUB,                "-");
    mk_proc(OP_MUL,                "*");
    mk_proc(OP_DIV,                "/");
    mk_proc(OP_ABS,                "abs");
    mk_proc(OP_QUOTIENT,           "quotient");
    mk_proc(OP_REMAINDER,          "remainder");
    mk_proc(OP_MODULO,             "modulo");
    // "gcd"
    // "lcm"
    // "numerator"
    // "denominator"
    mk_proc(OP_FLOOR,              "floor");
    mk_proc(OP_CEILING,            "ceiling");
    mk_proc(OP_TRUNCATE,           "truncate");
    mk_proc(OP_ROUND,              "round");
    // "rationalize"
    mk_proc(OP_EXP,                "exp");
    mk_proc(OP_LOG,                "log");
    mk_proc(OP_SIN,                "sin");
    mk_proc(OP_COS,                "cos");
    mk_proc(OP_TAN,                "tan");
    mk_proc(OP_ASIN,               "asin");
    mk_proc(OP_ACOS,               "acos");
    mk_proc(OP_ATAN,               "atan");
    mk_proc(OP_SQRT,               "sqrt");
    mk_proc(OP_EXPT,               "expt");
    /* complex numbers */
    // "make-rectangular"
    // "make-polar"
    // "real-part"
    // "imag-part"
    // "magnitude"
    // "angle"

    mk_proc(OP_EXACT_INEXACT,      "exact->inexact");
    mk_proc(OP_INEXACT_EXACT,      "inexact->exact");

    /**** 6.5.6. Numerical input and output ****/
    mk_proc(OP_NUMBER_TO_STRING,   "number->string");
    mk_proc(OP_STRING_TO_NUMBER,   "string->number");

    /**** 6.6. Characters ****/
    mk_proc(OP_CHARP,              "char?");
    mk_proc(OP_CHAR_EQ,            "char=?");
    mk_proc(OP_CHAR_LT,            "char<?");
    mk_proc(OP_CHAR_GT,            "char>?");
    mk_proc(OP_CHAR_LE,            "char<=?");
    mk_proc(OP_CHAR_GE,            "char>=?");
    mk_proc(OP_CHAR_CI_EQ,         "char-ci=?");
    mk_proc(OP_CHAR_CI_LT,         "char-ci<?");
    mk_proc(OP_CHAR_CI_GT,         "char-ci>?");
    mk_proc(OP_CHAR_CI_LE,         "char-ci<=?");
    mk_proc(OP_CHAR_CI_GE,         "char-ci>=?");
    mk_proc(OP_CHAR_ALPHAP,        "char-alphabetic?");
    mk_proc(OP_CHAR_NUMBERP,       "char-numeric?");
    mk_proc(OP_CHAR_WSP,           "char-whitespace?");
    mk_proc(OP_CHAR_UPPERP,        "char-upper-case?");
    mk_proc(OP_CHAR_LOWERP,        "char-lower-case?");
    mk_proc(OP_CHAR_INT,           "char->integer");
    mk_proc(OP_INT_CHAR,           "integer->char");
    mk_proc(OP_CHAR_UPCASE,        "char-upcase");
    mk_proc(OP_CHAR_DOWNCASE,      "char-downcase");

    /**** 6.7. Strings ****/
    mk_proc(OP_STRINGP,            "string?");
    mk_proc(OP_MAKE_STRING,        "make-string");
    mk_proc(OP_STRING,             "string");
    mk_proc(OP_STRING_LENGTH,      "string-length");
    mk_proc(OP_STRING_REF,         "string-ref");
    mk_proc(OP_STRING_SET,         "string-set!");
    mk_proc(OP_STRING_EQ,          "string=?");
    mk_proc(OP_STRING_CI_EQ,       "string-ci=?");
    mk_proc(OP_STRING_LT,          "string<?");
    mk_proc(OP_STRING_CI_LT,       "string-ci<?");
    mk_proc(OP_STRING_GT,          "string>?");
    mk_proc(OP_STRING_CI_GT,       "string-ci>?");
    mk_proc(OP_STRING_LE,          "string<=?");
    mk_proc(OP_STRING_CI_LE,       "string-ci<=?");
    mk_proc(OP_STRING_GE,          "string>=?");
    mk_proc(OP_STRING_CI_GE,       "string-ci>=?");
    mk_proc(OP_SUBSTRING,          "substring");
    mk_proc(OP_STRING_APPEND,      "string-append");
    mk_proc(OP_STRING_LIST,        "string->list");
    mk_proc(OP_LIST_STRING,        "list->string");
    mk_proc(OP_STRING_COPY,        "string-copy");
    mk_proc(OP_STRING_FILL,        "string-fill!");

    mk_proc(OP_STRING_NULL,        "string-null?");
    // mk_proc(OP_STRING_JOIN,        "string-join");

    /**** 6.8. Vectors ****/
    mk_proc(OP_VECTORP,            "vector?");
    mk_proc(OP_MAKE_VECTOR,        "make-vector");
    mk_proc(OP_VECTOR,             "vector");
    mk_proc(OP_VECTOR_LENGTH,      "vector-length");
    mk_proc(OP_VECTOR_REF,         "vector-ref");
    mk_proc(OP_VECTOR_SET,         "vector-set!");
    mk_proc(OP_VECTOR_LIST,        "vector->list");
    mk_proc(OP_LIST_VECTOR,        "list->vector");
    mk_proc(OP_VECTOR_FILL,        "vector-fill!");

    /**** 6.9. Control features ****/
    mk_proc(OP_PROC,               "procedure?");
    mk_proc(OP_PAPPLY,             "apply");
    // "map"        // in init.scm
    // "for-each"   // in init.scm
    mk_proc(OP_FORCE,              "force");
    mk_proc(OP_CONTINUATION,       "call-with-current-continuation");
    mk_proc(OP_CONTINUATION,       "call/cc");

    /**** 6.10.1. Ports ****/
    //mk_proc(OP_CALL_WITH_IN_FILE, "call-with-input-file");
    //mk_proc(OP_CALL_WITH_OUT_FILE, "call-with-output-file");
    mk_proc(OP_INPUTP,             "input-port?");
    mk_proc(OP_OUTPUTP,            "output-port?");
    mk_proc(OP_CURRENT_IN_PORT,    "current-input-port");
    mk_proc(OP_CURRENT_OUT_PORT,   "current-output-port");
    //mk_proc(OP_WITH_IN_FROM_FILE,  "with-input-from-file");
    //mk_proc(OP_WITH_OUT_FROM_FILE, "with-output-to-file");
    //mk_proc(OP_OPEN_INFILE,        "open-input-file");
    //mk_proc(OP_OPEN_OUTFILE,       "open-output-file");
    //mk_proc(OP_CLOSE_INFILE,       "close-input-file");
    //mk_proc(OP_CLOSE_OUTFILE,      "close-output-file");

    /**** 6.10.2. Input ****/
    mk_proc(OP_READ,               "read");
    //mk_proc(OP_READ_CHAR,          "read-char");
    //mk_proc(OP_PEEK_CHAR,          "peek-char");
    //mk_proc(OP_EOFP,               "eof-object?");
    //mk_proc(OP_CHAR_READYP,        "char-ready?");

    /**** 6.10.3. Output ****/
    mk_proc(OP_WRITE,              "write");
    mk_proc(OP_DISPLAY,            "display");
    mk_proc(OP_NEWLINE,            "newline");
    mk_proc(OP_WRITE_CHAR,         "write-char");

    /**** 6.10.4. System interface ****/
    mk_proc(OP_LOAD,               "load");
    // "transcript-on"
    // "transcript-off"

    /**** non-standard functions ****/
    mk_proc(OP_LOCALE_SET,         "locale-set!");
    mk_proc(OP_CLOCK,              "clock");
    mk_proc(OP_TIME,               "time");
    mk_proc(OP_TIME_STRING,        "time->string");
    mk_proc(OP_TIME_GMT,           "time->gmt");
    mk_proc(OP_SEED,               "seed");
    mk_proc(OP_ROLL,               "roll");

    mk_proc(OP_PEVAL,              "eval");
    mk_proc(OP_ERR0,               "error");
    mk_proc(OP_PUT,                "put");
    mk_proc(OP_GET,                "get");
    mk_proc(OP_GC,                 "gc");
    mk_proc(OP_GCVERB,             "gc-verbose");
    mk_proc(OP_NEWSEGMENT,         "new-segment");
    mk_proc(OP_PRINT_WIDTH,        "print-width");
    mk_proc(OP_GET_CLOSURE,        "get-closure-code");
    mk_proc(OP_CLOSUREP,           "closure?");
    mk_proc(OP_MACROP,             "macro?");
    mk_proc(OP_GENSYM,             "gensym");
    mk_proc(OP_QUIT,               "quit");
}

/* initialize several globals */
static void init_globals()
{
    init_vars_global();
    init_syntax();
    init_procs();

    /* intialization of global pointers to special symbols */
    LAMBDA    = mk_symbol("lambda");
    QUOTE     = mk_symbol("quote");
    QQUOTE    = mk_symbol("quasiquote");
    UNQUOTE   = mk_symbol("unquote");
    UNQUOTESP = mk_symbol("unquote-splicing");
    FEED_TO   = mk_symbol("=>");
}

/* initialization of Mini-Scheme */
static void init_scheme()
{
    if (alloc_cellseg(FIRST_CELLSEGS) != FIRST_CELLSEGS)
        fatal_error("Unable to allocate initial cell segments");
#ifdef VERBOSE
    gc_verbose = 1;
#else
    gc_verbose = 0;
#endif
    init_globals();

    linenoiseSetEncodingFunctions(linenoiseUtf8PrevCharLen,
                                  linenoiseUtf8NextCharLen,
                                  linenoiseUtf8ReadCode);
}

/* ========== Main ========== */

int main(int argc, char** argv)
{
    opcode op = OP_LOAD;

    init_scheme();

    if (argc == 1) {
        args = cons(mk_string(InitFile), NIL);
    } else {
        args = cons(mk_string(argv[1]), NIL);
    }

    /* setjmp() returns 0 when called directly so OP_LOAD
       needs to be the first or it breaks loading InitFile */
    op = setjmp(error_jmp);
    Eval_Cycle(op);

    ParseFree(parser, free);

    return 0;
}

