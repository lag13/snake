# TODO: What are the best practices for compiling C programs while
# developing? Is it a Makefile? Speaking of which, there is a LOT of
# repetition in this Makefile, can I cut that down somehow? Also, this
# Makefile is not perfect because I keep struct definitions in my
# header files so if those change then the corresponding .c file
# should be recompiled.
all: run

run: build
	./a.out

build: main.o stdlibutil.o pthreadutil.o pos.o dir.o charactionmap.o snake.o
	gcc -l ncurses main.o stdlibutil.o pthreadutil.o pos.o dir.o charactionmap.o snake.o

main.o: main.c
	gcc -c -Wall main.c

stdlibutil.o: stdlibutil.c
	gcc -c -Wall stdlibutil.c

pthreadutil.o: pthreadutil.c
	gcc -c -Wall pthreadutil.c

pos.o: pos.c
	gcc -c -Wall pos.c

dir.o: dir.c
	gcc -c -Wall dir.c

charactionmap.o: charactionmap.c
	gcc -c -Wall charactionmap.c

snake.o: snake.c
	gcc -c -Wall snake.c

.PHONY: clean
clean:
	rm *.o && rm a.out
