// This module is where the game logic for snake is defined.
#ifndef SNAKE_H
#define SNAKE_H

// bool
#include <stdbool.h>
// uint8_t
#include <stdint.h>
// pthread_mutex_t
#include <pthread.h>
// size_t
#include <stddef.h>

// pos
// posList
#include "pos.h"
// dir
#include "dir.h"

// These are the possible actions a player can take in the game of
// snake.
typedef enum {
  NO_ACTION,
  UP,
  DOWN,
  LEFT,
  RIGHT,
  PAUSE,
  QUIT,
  NEW_GAME
} playeraction;

// Returns a string describing what a specific player action does.
// This can be used to when displaying what controls are possible in
// the game.
char *playeraction_toString(playeraction p);

// A thread safe queue of playeraction's with a fixed maximum size of
// 8. Typically a queue is implemented as a linked list but an array
// works just as well and has the benefit of being more memory
// compact. You'll also notice that I'm using the "bit fields" feature
// since I haven't got the chance to try this convenience and because
// memory optimization is fun (update: it's a pretty neat feature!!).
// The game loop will pull these "player actions" off the queue in
// order to advance the game. Doing so makes the game agnostic to both
// where the input is coming from (keyboard, controller, etc...) and
// what the exact "raw" inputs are (maybe one person uses 'w' to move
// UP and someone else uses the up arrow). Since games (especially
// snake) will typically consume input faster than a player can
// produce, 8 slots seems more than satisfactory.
typedef struct {
  uint8_t head: 3;
  uint8_t tail: 3;
  uint8_t isFull: 1;
  // This pointer MUST point to an array with exactly 8 slots in it
  // otherwise this queue does not work.
  playeraction *arr;
  pthread_mutex_t *mu;
} playeractionQueue;

// How much space the backing array for the queue occupies. This is
// here for convenience so callers can more easily allocate the memory
// needed for the backing array for this queue.
extern const size_t PLAYERACTIONQUEUE_SPACEREQUIRED;

// Adds an item to the queue. If there is no more room on the queue
// then the it is discarded. This "lossy" behavior seems fine since
// the limit of the queue should never be reached in practice.
void playeractionQueue_enqueue(playeractionQueue *q, playeraction c);

// Returns how much memory is required for the game of snake. It is a
// convenience so callers can allocate the necessary amount of memory.
size_t snakeSpaceRequired(int width, int height);

// Plays the game of snake. The last parameter is a function pointer
// which is how C lets you pass functions around as parameters. For
// example if you have "char (*funcPtr)(int n)" then this means that
// the function pointer "funcPtr" can point to a function which
// returns a "char" and takes an "int" as a parameter. It's very messy
// to look at for sure.
void snake(int width, int height, void *snakeMem, playeractionQueue *actionsQueue, void (*render)(int width, int height, posList snake, pos food, bool paused, int score, bool gameIsWon, bool gameIsLost));

#endif	/* SNAKE_H */
