// initscr()
// mvaddch()
// etc...
#include <ncurses.h>
// timespec
#include <time.h>
// strlen()
#include <string.h>
// free()
#include <stdlib.h>
// pthread_t
#include <pthread.h>

// mustMalloc()
#include "stdlibutil.h"
// must_pthread_mutex_init()
#include "pthreadutil.h"
// pos
// posList
#include "pos.h"
// dir
#include "dir.h"
// playeractionQueue
// snake()
#include "snake.h"

void drawCharOnGrid(int x, int y, char c) {
  // On my terminal doubling the width seems to makes the game look
  // more proportional. I should look into how common this is (just
  // for curiosity). Could you configure your terminal to be more
  // proportional without needing to do something like this? Probably.
  mvaddch(y, 2*x, c);
}

void drawGrid(int width, int height) {
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      drawCharOnGrid(x, y, '.');
    }
  }
}

void drawStrOnGrid(int x, int y, char* s) {
  for (int i = 0; i < strlen(s); i++) {
    drawCharOnGrid(x+i, y, s[i]);
  }
}

void drawSnake(posList snake) {
  for (int i = 0; i < snake.len-1; i++) {
    drawCharOnGrid(snake.list[i].x, snake.list[i].y, '#');
  }
  drawCharOnGrid(snake.list[snake.len-1].x, snake.list[snake.len-1].y, '@');
}

void drawFood(pos food) {
  drawCharOnGrid(food.x, food.y, '*');
}

void drawPausedMsg(int width, int height, bool paused) {
  if (paused) {
    char* pauseMsg = "[Paused]";
    drawStrOnGrid(width/2 - strlen(pauseMsg)/2, height/2, pauseMsg);
  }
}

void drawScore(int height, int score) {
  mvprintw(height, 0, "Score: %d", score);
}

void drawGameOverText(int height, bool wonGame) {
  char* s;
  if (wonGame) {
    s = "Holy shit you won!!";
  } else {
    s = "You lost. Then again one usually \"loses\" at snake";
  }
  mvprintw(height+1, 0, s);
}

// It turns out that ncurses is not thread safe so we need to have
// this mutex to prevent multiple threads from executing nucurses
// functions at the same time:
// http://www.linuxmisc.com/9-unix-programmer/87e7383a80449bf7.htm
pthread_mutex_t ncursesMu;

void render(int width, int height, posList snake, pos food, bool paused, int score, bool gameIsWon, bool gameIsLost) {
  pthread_mutex_lock(&ncursesMu);
  clear();
  drawGrid(width, height);
  drawSnake(snake);
  drawFood(food);
  drawPausedMsg(width, height, paused);
  drawScore(height, score);
  if (gameIsWon || gameIsLost) {
    drawGameOverText(height, gameIsWon);
  }
  // TODO: Display the controls. To do this we need to create a hash
  // table of char -> playeraction.
  refresh();
  pthread_mutex_unlock(&ncursesMu);
}

// TODO: Pass this variable into the getInput function instead of
// having it be a global variable.
playeractionQueue *queue;

void *getInput() {
  while (1) {
    pthread_mutex_lock(&ncursesMu);
    char c = getch();
    pthread_mutex_unlock(&ncursesMu);
    if (c == 'w') {
      playeractionQueue_enqueue(queue, UP);
      continue;
    }
    if (c == 's') {
      playeractionQueue_enqueue(queue, DOWN);
      continue;
    }
    if (c == 'a') {
      playeractionQueue_enqueue(queue, LEFT);
      continue;
    }
    if (c == 'd') {
      playeractionQueue_enqueue(queue, RIGHT);
      continue;
    }
    if (c == 'p') {
      playeractionQueue_enqueue(queue, PAUSE);
      continue;
    }
    if (c == 'q') {
      playeractionQueue_enqueue(queue, QUIT);
      continue;
    }
    if (c == 'n') {
      playeractionQueue_enqueue(queue, NEW_GAME);
      continue;
    }
  }
}

int main(int argc, char** argv) {
  int width = 15;
  int height = 15;
  // This points to all allocated memory used by this game. The
  // benefit of this is that memory is allocated and free'd only once.
  // This variable need not exist but I liked the idea of having a
  // "memory bookeeping" sort of variable which is independent of any
  // particular application. TODO: This game is so small that it
  // really doesn't matter but is it bad practice to allocate one
  // giant contiguous chunk of memory? I guess it probably depends on
  // how big the chunk is.
  void* mem;

  {
    size_t snakeSpaceOccupies = snakeSpaceRequired(width, height);
    mem = mustMalloc(snakeSpaceOccupies + PLAYERACTIONQUEUE_SPACEREQUIRED);
    pthread_mutex_t mu;
    must_pthread_mutex_init(&mu, NULL);
    queue = &(playeractionQueue) { .arr = (mem+snakeSpaceOccupies), .mu = &mu };
  }

  {
    // initializes ncurses:
    // http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/
    initscr();
    // makes it so input is immediately sent to the program instead of
    // waiting for a newline (oddly though when testing this wasn't
    // necessary but I'm keeping it because that's what the docs say).
    cbreak();
    // prevents player input from appearing on screen
    noecho();
    // hides the cursor
    curs_set(0);
    // so calls to getch() do not block
    timeout(0);
    must_pthread_mutex_init(&ncursesMu, NULL);
    pthread_t threadID;
    must_pthread_create(&threadID, NULL, &getInput, NULL);
  }

  snake(width, height, mem, queue, render);

  endwin();
  free(mem);
}
