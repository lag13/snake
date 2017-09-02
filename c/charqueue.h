// Defines a thread safe queue of char's with a fixed maximum size of
// 8. This queue was made for one purpose, to hold raw player input
// before the game consumes it. Since games (especially snake) will
// typically consume input faster than a player can produce it, 8
// slots seems more than satisfactory.
#ifndef CHARQUEUE_H
#define CHARQUEUE_H

// uint8_t
#include <stdint.h>
// pthread_mutex_t
#include <pthread.h>
// size_t
#include <stddef.h>

// A queue of char's. Typically a queue is implemented as a linked
// list but an array works just as well and has the benefit of being
// more memory compact. You'll also notice that I'm using the "bit
// fields" feature since I haven't got the chance to try this
// convenience and because memory optimization is fun (update: it's a
// pretty neat feature!!).
typedef struct {
  uint8_t head: 3;
  uint8_t tail: 3;
  uint8_t isFull: 1;
  // This pointer MUST point to an array with exactly 8 slots in it.
  char *arr;
  pthread_mutex_t *mu;
} charQueue;

// How much space the backing array for the queue occupies. This is
// here for convenience so callers can more easily allocate the memory
// needed for the backing array for this queue.
extern const size_t CHARQUEUE_SPACEOCCUPIED;

// The value returned from the queue when it is empty.
extern const char CHARQUEUE_NOTHING;

// Returns the char at the beginning of the queue. If nothing is on
// the queue then CHARQUEUE_NOTHING will be returned.
char charQueue_dequeue(charQueue *q);

// Adds a char to the queue. If there is no more room on the queue
// then the char is discarded. This "lossy" behavior seems fine since
// the limit of the queue should never be reached in practice.
void charQueue_enqueue(charQueue *q, char c);

#endif	/* CHARQUEUE_H */
