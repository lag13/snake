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
