all: snake

run: snake
	./snake

snake: snake.o util.o
	gcc -l ncurses snake.o util.o -o snake

snake.o: snake.c
	gcc -c -Wall snake.c

util.o: util.c
	gcc -c -Wall util.c

.PHONY: clean
clean:
	rm *.o
