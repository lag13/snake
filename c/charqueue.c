#include "charqueue.h"

static const char maxSize = 8;
const size_t CHARQUEUE_SPACEOCCUPIED = sizeof(sizeof(char) * maxSize);
const char CHARQUEUE_NOTHING = 0;

void charQueue_enqueue(charQueue *q, char c) {
  if (q->isFull) {
    return;
  }
  pthread_mutex_lock(q->mu);
  q->arr[q->tail] = c;
  q->tail = (q->tail+1) % maxSize;
  if (q->tail == q->head) {
    q->isFull = 1;
  }
  pthread_mutex_unlock(q->mu);
}

char charQueue_dequeue(charQueue *q) {
  // if the head and tail point to the same place then the queue is
  // either full or empty.
  if (q->head == q->tail && !q->isFull) {
    return CHARQUEUE_NOTHING;
  }
  pthread_mutex_lock(q->mu);
  char elem = q->arr[q->head];
  q->head = (q->head+1) % maxSize;
  q->isFull = 0;
  pthread_mutex_unlock(q->mu);
  return elem;
}

