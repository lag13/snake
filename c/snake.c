// srand()
// rand()
#include <stdlib.h>
// time()
#include <time.h>

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

static const dir NO_DIRECTION = { .x = 0, .y = 0 };

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

void initState(int width, int height, posList *snake, pos *food, playeractionQueue *queue, dir *curDirection, bool *paused, int *score) {
  srand(time(NULL));
  snake->len = 4;
  snake->list[0] = (pos){ .x = 0, .y = 1 };
  snake->list[1] = (pos){ .x = 0, .y = 0 };
  snake->list[2] = (pos){ .x = 1, .y = 0 };
  snake->list[3] = (pos){ .x = 1, .y = 1 };
  *food = createFood(width, height, *snake);
  while (playeractionQueue_dequeue(queue) != NO_ACTION);
  *curDirection = actionToDirection(DOWN);
  *paused = false;
  *score = 0;
}

void updateGameState(playeraction action, int width, int height, bool *paused, posList *snake, pos *food, dir *d, int *score) {
  if (action == PAUSE) {
    *paused = !(*paused);
    return;
  }
  if (*paused) {
    return;
  }
  dir newDir = actionToDirection(action);
  if (!dir_equal(newDir, NO_DIRECTION)) {
    *d = newDir;
  }
  pos newHead = { .x = snake->list[snake->len-1].x + d->x, .y = snake->list[snake->len-1].y + d->y };
  if (pos_equal(newHead, *food)) {
    *snake = posList_append(*snake, newHead);
    *food = createFood(width, height, *snake);
    (*score)++;
  } else {
    for (int i = 0; i < snake->len-1; i++) {
      snake->list[i] = snake->list[i+1];
    }
    snake->list[snake->len-1].x += d->x;
    snake->list[snake->len-1].y += d->y;
  }
}
