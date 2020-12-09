# just build the thing fast
#CFLAGS = -Wall -lm -DNDEBUG

# smallest size
CFLAGS = -Os -s -Wall -Wl,-z,norelro -DNDEBUG -lm

# debug
#CFLAGS = -g -Wall -lm

all : yocto

yocto : Makefile yocto.c yocto.h yocto-lexer yocto-parser utf8
	$(CC) $(CFLAGS) -o yocto yocto-lexer.c yocto-parser.c linenoise/linenoise.c str/str.c utf8.o utf8-char-class.o yocto.c

yocto-lexer : yocto-lexer.re
	re2c -i -o yocto-lexer.c yocto-lexer.re

yocto-parser : lemon yocto-parser.y
	./lemon -q -l yocto-parser.y

utf8 : Makefile utf8.c utf8.h utf8-char-class.c utf8-char-class.h
	$(CC) $(CFLAGS) -c utf8-char-class.c utf8.c

lemon : lemon.c lempar.c
	$(CC) -o lemon lemon.c

clean :
	rm -f yocto-lexer.c yocto-parser.c yocto-parser.h utf8.o utf8-char-class.o lemon yocto

