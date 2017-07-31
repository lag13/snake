#ifndef _UTIL_H
#define _UTIL_H

void *mustMalloc(size_t size);

typedef struct dir {
  char x;
  char y;
} dir;

static const dir UP = { .x = 0, .y = -1 };
static const dir DOWN = { .x = 0, .y = 1 };
static const dir LEFT = { .x = -1, .y = 0 };
static const dir RIGHT = { .x = 1, .y = 0 };
static const dir PAUSED = { .x = 0, .y = 0 };

bool dir_equal(dir a, dir b);

typedef struct pos {
  char x;
  char y;
} pos;

typedef struct posList {
  int len;
  pos *list;
} posList;

pos createFood(int width, int height, posList snake);
void updateGameState(int width, int height, posList *snake, pos *food, dir curDirection, int *score);
bool gameIsOver(int width, int height, posList snake);
bool gameIsWon(int width, int height, posList snake);

#endif
