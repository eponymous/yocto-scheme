# just build the thing fast
#CFLAGS = -Wall -lm -DNDEBUG

# smallest size
CFLAGS = -Os -s -Wall -Wl,-z,norelro -DNDEBUG -lm

# debug
#CFLAGS = -g -Wall -lm

all : yocto

yocto : yocto.c yocto.h yocto-lexer.c yocto-parser.c Makefile
	$(CC) $(CFLAGS) -o yocto yocto-lexer.c yocto-parser.c linenoise/linenoise.c yocto.c

yocto-lexer.c : yocto-lexer.re
	re2c -i -o yocto-lexer.c yocto-lexer.re

yocto-parser.c : lemon yocto-parser.y
	./lemon -q -l yocto-parser.y

lemon : lemon.c
	$(CC) -o lemon lemon.c

clean :
	rm -f yocto-lexer.c yocto-parser.c yocto-parser.h lemon yocto

