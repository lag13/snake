// TODO: Make a graphical version of this program.

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
// charactionPair
// charactionMap
#include "charactionmap.h"
// playeractionQueue
// snake()
#include "snake.h"

void drawCharOnGrid(int x, int y, char c) {
  // On my terminal doubling the width seems to makes the game look
  // more proportional. TODO: Could you configure your terminal to be
  // more proportional without needing to do something like this?
  // Probably.
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

void drawControls(int width, charactionMap controls) {
  for (int y = 0; y < controls.len; y++) {
    charactionPair pair = controls.map[y];
    mvaddch(y, 2*width+1, pair.key);
    mvaddch(y, 2*width+3, '-');
    mvprintw(y, 2*width+5, playeraction_toString(pair.value));
  }
}

// TODO: Learn more about threads. What I want to figure out is: 1.
// Will the thread reading input stop automatically when main stops or
// do I need to cancel it (I'm fairly certain it will stop when main
// does). 2. I want to pass the playeractionQueue as an argument to
// the function reading input rather than having it as a global
// variable and I want to know if doing that is okay? I read somewhere
// that it is *potentially* unsafe because the main thread (where the
// queue was defined) could stop and then at the same time the thread
// getting input could try to access that now non-existant variable
// causing some sort of failure. 3. I want to learn more about how
// goroutines are implemented and if I could implement something like
// them in C. I wonder if that would be hard or if that would have
// benefits in terms of memory and such (and how would I even measure
// those sorts of things). 4. I briefly read about the library openMP
// which, like pthreads, also allows for doing parallel things. I
// would be interested in trying that out.

// Links:
// https://stackoverflow.com/questions/16230542/passing-multiple-arguments-to-threaded-function-from-pthread-create.
// https://en.wikipedia.org/wiki/Thread_(computing)
// https://computing.llnl.gov/tutorials/parallel_comp
// https://computing.llnl.gov/tutorials/pthreads
// https://computing.llnl.gov/tutorials/openMP
// https://stackoverflow.com/questions/27789227/what-exactly-are-goroutines
// https://dave.cheney.net/2014/06/07/five-things-that-make-go-fast
// https://stackoverflow.com/questions/11875956/main-thread-exit-does-other-exit-too
// https://stackoverflow.com/questions/19744250/what-happens-to-a-detached-thread-when-main-exits
// https://stackoverflow.com/questions/16230542/passing-multiple-arguments-to-threaded-function-from-pthread-create

// It turns out that ncurses is not thread safe so we need to have
// this mutex to prevent multiple threads from executing nucurses
// functions at the same time:
// http://www.linuxmisc.com/9-unix-programmer/87e7383a80449bf7.htm
pthread_mutex_t ncursesMu;

const uint8_t mapLen = 7;
charactionMap controls = {
  .len = mapLen,
  .map = (charactionPair[mapLen]){{'w', UP}, {'s', DOWN}, {'a', LEFT}, {'d', RIGHT}, {'p', PAUSE}, {'q', QUIT}, {'n', NEW_GAME}}
};

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
  drawControls(width, controls);
  refresh();
  pthread_mutex_unlock(&ncursesMu);
}

playeractionQueue *queue;

void *getInput() {
  bool exists;
  playeraction action;
  while (1) {
    pthread_mutex_lock(&ncursesMu);
    char c = getch();
    pthread_mutex_unlock(&ncursesMu);
    exists = charactionMap_getValue(controls, c, &action);
    if (exists) {
      playeractionQueue_enqueue(queue, action);
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
    // The calls to getch() happen in another thread but since all
    // ncurses operations are protected by a mutex the getch()
    // function cannot block because that would prevent other ncurses
    // functions from running.
    timeout(0);
    must_pthread_mutex_init(&ncursesMu, NULL);
    pthread_t threadID;
    must_pthread_create(&threadID, NULL, &getInput, NULL);
  }

  snake(width, height, mem, queue, render);
  // TODO: Should I register some Ctrl-c handlers and what not? I
  // would imagine not in this case since if this program stops then
  // the memory is going to be cleaned up anyway? I wonder if ncurses
  // does this so that endwin() always gets closed because if it
  // doesn't I think it messes up the terminal.
  endwin();
  free(mem);
  return 0;
}
