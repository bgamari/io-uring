CFLAGS = -Wall -Werror -g3 -ggdb -I../include

all : test

syscall.o : ../cbits/syscall.c
	gcc ${CFLAGS} -c $< -o $@
hs_uring.o : ../cbits/hs_uring.c
	gcc ${CFLAGS} -c $< -o $@
test : test.cpp syscall.o hs_uring.o
	g++ ${CFLAGS} -o $@ $+
