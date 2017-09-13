// srand()
// rand()
// exit()
#include <stdlib.h>
// time()
#include <time.h>

#include "snake.h"

static playeraction playeractionQueue_dequeue(playeractionQueue *q) {
  // if the head and tail point to the same place then the queue is
  // either full or empty.
  if (q->head == q->tail && !q->isFull) {
    return NO_ACTION;
  }
  pthread_mutex_lock(q->mu);
  playeraction elem = q->arr[q->head];
  q->head = (q->head+1) % PLAYERACTIONQUEUE_SIZE;
  q->isFull = 0;
  pthread_mutex_unlock(q->mu);
  return elem;
}

static pos createFood(int width, int height, posList snake) {
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

static const dir NO_DIRECTION = { .x = 0, .y = 0 };

static dir actionToDirection(playeraction a) {
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

static void initState(int width, int height, posList *snake, pos *food, playeractionQueue *queue, dir *curDirection, bool *paused, int *score) {
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

static bool gameIsWon(int width, int height, posList snake) {
  return width*height == snake.len;
}

static bool gameIsLost(int width, int height, posList snake) {
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

static void updateGameState(playeraction action, int width, int height, bool *paused, posList *snake, pos *food, dir *d, int *score) {
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

char *playeraction_toString(playeraction p) {
  switch (p) {
  case NO_ACTION:
    return "no action";
  case UP:
    return "move the snake up";
  case DOWN:
    return "move the snake down";
  case LEFT:
    return "move the snake left";
  case RIGHT:
    return "move the snake right";
  case PAUSE:
    return "pause the game";
  case QUIT:
    return "quit the game";
  case NEW_GAME:
    return "start a new game";
  default:
    return "BUG!!! Unknown player action, please add another case to the switch statement";
  }
}

void playeractionQueue_enqueue(playeractionQueue *q, playeraction c) {
  if (q->isFull) {
    return;
  }
  pthread_mutex_lock(q->mu);
  q->arr[q->tail] = c;
  q->tail = (q->tail+1) % PLAYERACTIONQUEUE_SIZE;
  if (q->tail == q->head) {
    q->isFull = 1;
  }
  pthread_mutex_unlock(q->mu);
}

size_t snakeSpaceRequired(int width, int height) {
  posList snake;
  return width * height * sizeof(*snake.list);
}

void snake(int width, int height, struct timespec frameRate, void *snakeMem, playeractionQueue *actionsQueue, void (*render)(int width, int height, posList snake, pos food, bool paused, int score, bool gameIsWon, bool gameIsLost)) {
  posList snake;
  pos food;
  dir curDirection;
  bool paused;
  int score;

  snake.list = snakeMem;
  
  initState(width, height, &snake, &food, actionsQueue, &curDirection, &paused, &score);

  while (true) {
    bool won = gameIsWon(width, height, snake);
    bool lost = gameIsLost(width, height, snake);
    render(width, height, snake, food, paused, score, won, lost);
    nanosleep(&frameRate, NULL);
    playeraction action = playeractionQueue_dequeue(actionsQueue);
    while (!dir_orthogonal(curDirection, actionToDirection(action))) {
      action = playeractionQueue_dequeue(actionsQueue);
    }
    if (action == QUIT) {
      break;
    }
    if (action == NEW_GAME) {
        initState(width, height, &snake, &food, actionsQueue, &curDirection, &paused, &score);
	continue;
    }
    if (won || lost) {
      continue;
    }
    updateGameState(action, width, height, &paused, &snake, &food, &curDirection, &score);
  }
}
