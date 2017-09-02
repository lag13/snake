all: run

run: build
	./a.out

build: main.o stdlibutil.o pos.o dir.o snake.o
	gcc -l ncurses main.o stdlibutil.o pos.o dir.o snake.o

main.o: main.c
	gcc -c -Wall main.c

stdlibutil.o: stdlibutil.c
	gcc -c -Wall stdlibutil.c

pos.o: pos.c
	gcc -c -Wall pos.c

dir.o: dir.c
	gcc -c -Wall dir.c

snake.o: snake.c
	gcc -c -Wall snake.c

.PHONY: clean
clean:
	rm *.o && rm a.out
