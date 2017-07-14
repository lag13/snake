all: snake

snake: snake.o
	gcc -l ncurses snake.o -o snake

snake.o: snake.c
	gcc -c -Wall snake.c

.PHONY: clean
clean:
	rm *.o
