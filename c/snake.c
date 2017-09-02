// rand()
#include <stdlib.h>

#include "snake.h"

pos createFood(int width, int height, posList snake) {
  // The high level view of how this code works: (1) Creates a list of
  // open positions (i.e positions not occuipied by the snake) and (2)
  // grabs a random element from that list. But it does this without
  // creating a list of open positions.
  int maxRand = width*height - snake.len;
  if (maxRand <= 0) {
    return (pos){ .x = -1, .y = -1 };
  }
  int randIndex = rand() % maxRand;
  int curRandIndex = 0;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      pos p = { .x = x, .y = y };
      if (posList_contains(snake, p)) {
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
    if (posList_contains(headless, snake.list[snake.len-1])) {
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

bool gameIsDone(int width, int height, posList snake) {
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
