#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include "util.h"

void* mustMalloc(size_t size) {
  void* ptr = malloc(size);
  if (ptr == NULL) {
    fprintf(stderr, "malloc(%zd): %s (%d)\n", size, strerror(errno), errno);
    exit(1);
  }
  return ptr;
}

const dir UP = { .x = 0, .y = -1 };
const dir DOWN = { .x = 0, .y = 1 };
const dir LEFT = { .x = -1, .y = 0 };
const dir RIGHT = { .x = 1, .y = 0 };
const dir PAUSED = { .x = 0, .y = 0 };

bool dir_equal(dir a, dir b) {
  return a.x == b.x && a.y == b.y;
}

bool pos_equal(pos a, pos b) {
  return a.x == b.x && a.y == b.y;
}

int posList_containsPos(posList pl, pos toFind) {
  for (int i = 0; i < pl.len; i++) {
    if (pos_equal(toFind, pl.list[i])) {
      return true;
    }
  }
  return false;
}

posList posList_append(posList pl, pos p) {
  pl.list[pl.len] = p;
  pl.len = pl.len + 1;
  return pl;
}

// createFood returns a new random food position that the snake does
// not occupy. The high level view is that it: (1) Creates a list of
// open positions (i.e positions not occuipied by the snake), (2)
// grabs a random element from that list. But it does this without
// creating a list of open positions.
pos createFood(int width, int height, posList snake) {
  int maxRand = width*height - snake.len;
  if (maxRand <= 0) {
    return (pos){ .x = -1, .y = -1 };
  }
  int randIndex = rand() % maxRand;
  int curRandIndex = 0;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      pos p = { .x = x, .y = y };
      if (posList_containsPos(snake, p)) {
	continue;
      }
      if (curRandIndex == randIndex) {
	return p;
      }
      curRandIndex++;
    }
  }
  // This should never be reached but I added it to get rid of the
  // compiler warning.
  return (pos){ .x = -1, .y = -1 };
}

bool gameIsWon(int width, int height, posList snake) {
  return width*height == snake.len;
}

bool gameIsLost(int width, int height, posList snake) {
  {
    posList headless = snake;
    headless.len--;
    if (posList_containsPos(headless, snake.list[snake.len-1])) {
      return true;
    }
  }
  {
    pos head = snake.list[snake.len-1];
    if (head.x < 0 || width <= head.x) {
      return true;
    }
    if (head.y < 0 || height <= head.y) {
      return true;
    }
  }
  return false;
}

bool gameIsOver(int width, int height, posList snake) {
  return gameIsWon(width, height, snake) || gameIsLost(width, height, snake);
}

void updateGameState(int width, int height, posList* snake, pos* food, dir d, int* score) {
  pos newHead = { .x = snake->list[snake->len-1].x + d.x, .y = snake->list[snake->len-1].y + d.y };
  if (pos_equal(newHead, *food)) {
    *snake = posList_append(*snake, newHead);
    *food = createFood(width, height, *snake);
    (*score)++;
  } else {
    for (int i = 0; i < snake->len-1; i++) {
      snake->list[i] = snake->list[i+1];
    }
    snake->list[snake->len-1].x += d.x;
    snake->list[snake->len-1].y += d.y;
  }
}
