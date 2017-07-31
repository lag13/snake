#include <ncurses.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

// drawCharOnGrid draws a character on the game grid.
void drawCharOnGrid(int x, int y, char c) {
  // On my terminal, doubling the width makes the game more
  // proportional
  mvaddch(y, 2*x, c);
}

void drawGrid(int width, int height) {
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      drawCharOnGrid(x, y, '.');
    }
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

void drawStrOnGrid(int x, int y, char* s) {
  for (int i = 0; i < strlen(s); i++) {
    drawCharOnGrid(x+i, y, s[i]);
  }
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

void render(int width, int height, posList snake, pos food, bool paused, int score) {
  drawGrid(width, height);
  drawSnake(snake);
  drawFood(food);
  drawPausedMsg(width, height, paused);
  drawScore(height, score);
  if (gameIsOver(width, height, snake)) {
    drawGameOverText(height, gameIsWon(width, height, snake));
  }
  refresh();
}

dir getInput(dir curDirection) {
  char c;
  // When there is no delay to recieve characters, ERR indicates that
  // no input was recieved.
  while ((c = getch()) != ERR) {
    if (c == 'w' && !dir_equal(curDirection, DOWN)) {
      return UP;
    }
    if (c == 's' && !dir_equal(curDirection, UP)) {
      return DOWN;
    }
    if (c == 'a' && !dir_equal(curDirection, RIGHT)) {
      return LEFT;
    }
    if (c == 'd' && !dir_equal(curDirection, LEFT)) {
      return RIGHT;
    }
    if (c == 'p') {
      return PAUSED;
    }
  }
  return curDirection;
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
  // particular application.
  void* mem;

  // initializes ncurses:
  // http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/
  initscr();
  // makes it so input is immediately sent to the program instead of
  // waiting for a newline (although when testing this wasn't
  // necessary)
  cbreak();
  // prevents input from appearing on screen
  noecho();
  // hides the cursor
  curs_set(0);
  // so calls to getch() do not block
  timeout(0);
  srand(time(NULL));
  width = 15, height = 15;
  curDirection = DOWN;
  paused = false;
  score = 0;
  mem = mustMalloc(width*height*sizeof(*snake.list));
  snake.len = 4;
  snake.list = mem;
  snake.list[0] = (pos){ .x = 0, .y = 1 };
  snake.list[1] = (pos){ .x = 0, .y = 0 };
  snake.list[2] = (pos){ .x = 1, .y = 0 };
  snake.list[3] = (pos){ .x = 1, .y = 1 };
  food = createFood(width, height, snake);

  struct timespec delay = {0, 100000000};
  while (!gameIsOver(width, height, snake)) {
    render(width, height, snake, food, paused, score);
    nanosleep(&delay, NULL);
    dir d = getInput(curDirection);
    if (dir_equal(d, PAUSED)) {
      paused = !paused;
    } else {
      curDirection = d;
    }
    if (!paused) {
      updateGameState(width, height, &snake, &food, curDirection, &score);
    }
  }

  render(width, height, snake, food, paused, score);
  // Eat unused input
  while(getch() != ERR);
  // Wait for the user to hit a key before quitting.
  timeout(-1);
  getch();
  endwin();
  free(mem);
}
