// Defines functions to manipulate the data that makes up a game of
// snake. One thing worth noting is that the snake is represented as
// an array and the head of the snake is always the LAST element of
// this array.
#ifndef SNAKE_H
#define SNAKE_H

// bool
#include <stdbool.h>

// pos
// posList
#include "pos.h"
// dir
#include "dir.h"
// playeraction
#include "playeraction.h"

// TODO: The playeractions module should probably be part of this one.

// Returns a new random food position that the snake does not occupy.
pos createFood(int width, int height, posList snake);

// Returns true if the game is won.
bool gameIsWon(int width, int height, posList snake);

// Returns true if the game is lost.
bool gameIsLost(int width, int height, posList snake);

// Returns true if the game is over with.
bool gameIsDone(int width, int height, posList snake);

// Initializes the state of the game. This does NOT allocate any
// memory. Any data structures which require memory should have had
// that memory allocated already.
void initState(int width, int height, posList *snake, pos *food, playeractionQueue *queue, dir *curDirection, bool *paused, int *score);

// Updates the state of the game.
void updateGameState(playeraction action, int width, int height, bool *paused, posList *snake, pos *food, dir *d, int *score);

// Converts an action into a direction. TODO: Feels like I'm starting
// to throw a lot of randomish stuff into this file. Is there a better
// way to organize this? At this point I think some of these functions
// aren't even used anymore.
dir actionToDirection(playeraction a);

#endif	/* SNAKE_H */

