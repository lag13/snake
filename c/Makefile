all: run

run: build
	./a.out

build: main.o stdlibutil.o pthreadutil.o pos.o dir.o snake.o playeraction.o
	gcc -l ncurses main.o stdlibutil.o pthreadutil.o pos.o dir.o snake.o playeraction.o

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

snake.o: snake.c
	gcc -c -Wall snake.c

playeraction.o: playeraction.c
	gcc -c -Wall playeraction.c

.PHONY: clean
clean:
	rm *.o && rm a.out
