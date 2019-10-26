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

#include <string.h>
#include <stdlib.h>

#include "yocto.h"
#include "yocto-parser.h"
#include "linenoise/linenoise.h"

extern cell *T;
extern cell *F;

static char *yytoken;
static char *yycursor;
static char *yylimit;

/* clear input buffer */
void clearinput()
{
    yycursor = yylimit = yytoken = line;
}

/* back to standard input */
void flushinput()
{
    if (infp != stdin) {
        fclose(infp);
        infp = stdin;
        is_interactive = 1;
    }

    if (line) {
        free(line);
        line = NULL;
    }

    clearinput();
}

static void yyfill(int need)
{
    if (yycursor >= yylimit) {  /* input buffer is empty */
        if (infp != stdin && !feof(infp)) {
            int free = yytoken - line;
            memmove(line, yytoken, yylimit - yytoken);
            yylimit  -= free;
            yycursor -= free;
            yytoken  -= free;

            while(yylimit < line + LINESIZE) {
                int c = fgetc(infp);
                *yylimit++ = (c == EOF ? '\0' : c);

                if (c == EOF)
                    break;
            }

            if ((yylimit - line) < need) {
                line[yylimit - line] = '\0';
            }
        } else {
            flushinput();

            if ((line = linenoise(PROMPT)) == NULL) {
                printf("\n");
                exit(0);
            } else {
                linenoiseHistoryAdd(line);
            }

            yycursor = yytoken = line;
            yylimit = line + strlen(line) + 1;
        }
    }
}

int lex(cell **value)
{
    int strlen;
    char *yymarker;
    yytoken = yycursor;

yy0:
    /*!re2c

    re2c:indent:top      = 1;
    re2c:define:YYCTYPE  = "char";
    re2c:define:YYCURSOR = yycursor;
    re2c:define:YYLIMIT  = yylimit;
    re2c:define:YYMARKER = yymarker;
    re2c:define:YYFILL   = yyfill;

    "("                  { return TOK_LPAREN;    }
    ")"                  { return TOK_RPAREN;    }
    "."                  { return TOK_DOT;       }
    "'"                  { return TOK_QUOTE;     }
    "`"                  { return TOK_QQUOTE;    }
    ","                  { return TOK_UNQUOTE;   }
    ",@"                 { return TOK_UNQUOTESP; }
    "#("                 { return TOK_VECTOR;    }

    "=>"                 { *value = mk_symbol("=>");                                   return TOK_SYMBOL; }
    "and"                { *value = mk_symbol("and");                                  return TOK_SYMBOL; }
    "begin"              { *value = mk_symbol("begin");                                return TOK_SYMBOL; }
    "case"               { *value = mk_symbol("case");                                 return TOK_SYMBOL; }
    "cond"               { *value = mk_symbol("cond");                                 return TOK_SYMBOL; }
    "define"             { *value = mk_symbol("define");                               return TOK_SYMBOL; }
    "delay"              { *value = mk_symbol("delay");                                return TOK_SYMBOL; }
    "do"                 { *value = mk_symbol("do");                                   return TOK_SYMBOL; }
    "else"               { *value = mk_symbol("else");                                 return TOK_SYMBOL; }
    "if"                 { *value = mk_symbol("if");                                   return TOK_SYMBOL; }
    "lambda"             { *value = mk_symbol("lambda");                               return TOK_SYMBOL; }
    "let"                { *value = mk_symbol("let");                                  return TOK_SYMBOL; }
    "let*"               { *value = mk_symbol("let*");                                 return TOK_SYMBOL; }
    "letrec"             { *value = mk_symbol("letrec");                               return TOK_SYMBOL; }
    "or"                 { *value = mk_symbol("or");                                   return TOK_SYMBOL; }
    "quasiquote"         { *value = mk_symbol("quasiquote");                           return TOK_SYMBOL; }
    "quote"              { *value = mk_symbol("quote");                                return TOK_SYMBOL; }
    "set!"               { *value = mk_symbol("set!");                                 return TOK_SYMBOL; }
    "unquote"            { *value = mk_symbol("unquote");                              return TOK_SYMBOL; }
    "unquote-splicing"   { *value = mk_symbol("unquote-splicing");                     return TOK_SYMBOL; }

    "#t" | "#true"       { *value = T;                                                 return TOK_BOOLEAN; }
    "#f" | "#false"      { *value = F;                                                 return TOK_BOOLEAN; }

    '#\\alarm'           { *value = mk_character(0x07);                                return TOK_CHARACTER; }
    '#\\backspace'       { *value = mk_character(0x08);                                return TOK_CHARACTER; }
    '#\\delete'          { *value = mk_character(0x5f);                                return TOK_CHARACTER; }
    '#\\escape'          { *value = mk_character(0x1b);                                return TOK_CHARACTER; }
    '#\\newline'         { *value = mk_character(0x0a);                                return TOK_CHARACTER; }
    '#\\null'            { *value = mk_character(0x00);                                return TOK_CHARACTER; }
    '#\\return'          { *value = mk_character(0x0d);                                return TOK_CHARACTER; }
    '#\\space'           { *value = mk_character(0x20);                                return TOK_CHARACTER; }
    '#\\tab'             { *value = mk_character(0x09);                                return TOK_CHARACTER; }
    "#\\x"[a-fA-F0-9]{2} { *value = mk_character(strtoul(yytoken + 3, &yycursor, 16)); return TOK_CHARACTER; }
    "#\\".               { *value = mk_character(*(yytoken + 2));                      return TOK_CHARACTER; }

    digit_2              = [01];
    digit_8              = [0-7];
    digit_10             = [0-9];
    digit_16             = [a-fA-F0-9];

    sign                 = [+-];

    real_10              = sign? digit_10+;
    decimal_10           = sign? (digit_10* "." digit_10+ | digit_10+ ".")([eE] real_10)?;

    num_2                = "#b" digit_2+;
    num_2                {
                             *value = mk_exact(strtoul(yytoken + 2, &yycursor, 2));
                             return TOK_NUMBER;
                         }

    num_8                = "#o" sign? digit_8+;
    num_8                {
                             *value = mk_exact(strtoul(yytoken + 2, &yycursor, 8));
                             return TOK_NUMBER;
                         }

    float_10             = "#d"? decimal_10;
    float_10             {
                             *value = mk_inexact(atof(*yytoken == '#' ? yytoken + 2 : yytoken));
                             return TOK_NUMBER;
                         }

    integer_10           = "#d"? real_10;
    integer_10           {
                             *value = mk_exact(atol(*yytoken == '#' ? yytoken + 2 : yytoken));
                             return TOK_NUMBER;
                         }

    num_16               = "#x" sign? digit_16+;
    num_16               {
                             *value = mk_exact(strtoul(yytoken + 2, &yycursor, 16));
                             return TOK_NUMBER;
                         }

    string               = ["]("\\\""|[^"])*["];
    string               {
                             strlen = yycursor - yytoken - 2;

                             if (strlen > LINESIZE - 1) {
                                  sprintf(strbuf, "string longer than max allowable string: %i", LINESIZE);
                                  error(strbuf);
                             }

                             strncpy(strbuf, yytoken + 1, strlen);
                             strbuf[strlen] = '\0';
                             *value = mk_string(strbuf);
                             return TOK_STRING;
                         }

    letter               = [a-zA-Z];
    special_initial      = ('!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^');
    special_subsequent   = ('.' | '+' | '-');
    peculiar_identifier  = ('+' | '-' | '...');

    initial              = letter | special_initial;
    subsequent           = initial | digit_10 | special_subsequent;
    identifier           = (initial subsequent*) | peculiar_identifier;

    identifier           {
                             strlen = yycursor - yytoken;

                             if (strlen > LINESIZE - 1) {
                                  sprintf(strbuf, "symbol longer than max allowable string: %i", LINESIZE);
                                  error(strbuf);
                             }

                             strncpy(strbuf, yytoken, strlen);
                             strbuf[strlen] = '\0';
                             *value = mk_symbol(strbuf);
                             return TOK_SYMBOL;
                         }

    intraline_whitespace = [ \t];
    line_ending          = [\n] | [\r][\n] | [\r];
    whitespace           = (intraline_whitespace | line_ending)+;
    whitespace           { yytoken = yycursor; goto yy0; }

    nested_comment       = "#|" ([^#|] | ("#" [^|]) | ("|" [^#]))* "|#";
    comment              = (';' [^\n\x00]* line_ending) | nested_comment;
    comment              { yytoken = yycursor; goto yy0; }

    eof                  = [\x00];
    eof                  { return 0; }

    *                    {
                             sprintf(strbuf, "parse error illegal character: %c", *yycursor);
                             error(strbuf);
                         }
    */
}

