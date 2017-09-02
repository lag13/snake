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

// Returns a new random food position that the snake does not occupy.
pos createFood(int width, int height, posList snake);

// Returns true if the game is won.
bool gameIsWon(int width, int height, posList snake);

// Returns true if the game is lost.
bool gameIsLost(int width, int height, posList snake);

// Returns true if the game is over with.
bool gameIsDone(int width, int height, posList snake);

// updates the state of the game
void updateGameState(int width, int height, posList* snake, pos* food, dir d, int* score);

#endif	/* SNAKE_H */

