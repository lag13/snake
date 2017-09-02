#include "playeraction.h"

static const char queueSize = 8;
const size_t PLAYERACTIONQUEUE_SPACEOCCUPIED = sizeof(sizeof(playeraction) * queueSize);

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
    return "paused the game";
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
  q->tail = (q->tail+1) % queueSize;
  if (q->tail == q->head) {
    q->isFull = 1;
  }
  pthread_mutex_unlock(q->mu);
}

playeraction playeractionQueue_dequeue(playeractionQueue *q) {
  // if the head and tail point to the same place then the queue is
  // either full or empty.
  if (q->head == q->tail && !q->isFull) {
    return NO_ACTION;
  }
  pthread_mutex_lock(q->mu);
  playeraction elem = q->arr[q->head];
  q->head = (q->head+1) % queueSize;
  q->isFull = 0;
  pthread_mutex_unlock(q->mu);
  return elem;
}


