// initscr()
// mvaddch()
// etc...
#include <ncurses.h>
// time()
// timespec
#include <time.h>
// strlen()
#include <string.h>
// srand()
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
#include "playeraction.h"
// gameIsDone()
// updateGameState()
// etc...
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
// functions at the same time.
pthread_mutex_t ncursesMu;

void render(int width, int height, posList snake, pos food, bool paused, int score) {
  pthread_mutex_lock(&ncursesMu);
  drawGrid(width, height);
  drawSnake(snake);
  drawFood(food);
  drawPausedMsg(width, height, paused);
  drawScore(height, score);
  if (gameIsDone(width, height, snake)) {
    drawGameOverText(height, gameIsWon(width, height, snake));
  }
  refresh();
  pthread_mutex_unlock(&ncursesMu);
}

const dir NO_DIRECTION = { .x = 0, .y = 0 };

dir actionToDirection(playeraction a) {
  if (a == UP) {
    return (dir){ .x = 0, .y = -1 };
  }
  if (a == DOWN) {
    return (dir){ .x = 0, .y = 1 };
  }
  if (a == LEFT) {
    return (dir){ .x = -1, .y = 0 };
  }
  if (a == RIGHT) {
    return (dir){ .x = 1, .y = 0 };
  }
  return NO_DIRECTION;
}

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
  }
}

int main(int argc, char** argv) {
  int width, height;
  posList snake;
  pos food;
  dir curDirection;
  bool paused;
  int score;
  // This points to all allocated memory used by this game. The
  // benefit of this is that memory is allocated and free'd only once.
  // This variable need not exist but I liked the idea of having a
  // "memory bookeeping" sort of variable which is independent of any
  // particular application. TODO: This game is so small that it
  // really doesn't matter but is it bad practice to allocate one
  // giant contiguous chunk of memory? I guess it probably depends on
  // how big the chunk is.
  void* mem;

  srand(time(NULL));
  width = 15, height = 15;
  curDirection = actionToDirection(DOWN);
  paused = false;
  score = 0;
  pthread_mutex_t mu;
  must_pthread_mutex_init(&mu, NULL);
  must_pthread_mutex_init(&ncursesMu, NULL);
  size_t snakeSpaceOccupies = width*height*sizeof(*snake.list);
  mem = mustMalloc(snakeSpaceOccupies + PLAYERACTIONQUEUE_SPACEOCCUPIED);
  snake.list = mem;
  queue = &(playeractionQueue) { .arr = (mem+snakeSpaceOccupies), .mu = &mu };
  snake.len = 4;
  snake.list[0] = (pos){ .x = 0, .y = 1 };
  snake.list[1] = (pos){ .x = 0, .y = 0 };
  snake.list[2] = (pos){ .x = 1, .y = 0 };
  snake.list[3] = (pos){ .x = 1, .y = 1 };
  food = createFood(width, height, snake);
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
  pthread_t threadID;
  must_pthread_create(&threadID, NULL, &getInput, NULL);

  struct timespec delay = {0, 100000000};
  while (!gameIsDone(width, height, snake)) {
    render(width, height, snake, food, paused, score);
    nanosleep(&delay, NULL);
    playeraction action = playeractionQueue_dequeue(queue);
    while (!dir_orthogonal(curDirection, actionToDirection(action))) {
      action = playeractionQueue_dequeue(queue);
    }
    dir newDir = actionToDirection(action);
    if (!dir_equal(newDir, NO_DIRECTION)) {
      curDirection = newDir;
    }
    if (action == PAUSE) {
      paused = !paused;
    }
    if (!paused) {
      updateGameState(width, height, &snake, &food, curDirection, &score);
    }
  }

  render(width, height, snake, food, paused, score);
  free(mem);
  endwin();
}
