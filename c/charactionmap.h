// This module defines a map from char's to player actions. This map
// was created for the purpose of making it simpler to add player
// actions to the queue which the snake game loop reads from and makes
// it possible to display all the possible actions you can take when
// playing snake.
#ifndef CHARACTIONMAP_H
#define CHARACTIONMAP_H

// bool
#include <stdbool.h>
// uint8_t
#include <stdint.h>
// size_t
#include <stddef.h>
// playeraction
#include "snake.h"

// This is map is really just a list of key+value pairs and this is
// the definition of those pairs.
typedef struct {
  char key;
  playeraction value;
} charactionPair;

// The map of char's to playeraction's.
typedef struct {
  uint8_t len;
  charactionPair *map;
} charactionMap;

// Returns how much memory is required for the map. It is a
// convenience so callers can more easily allocate the necessary
// amount of memory.
size_t charactionMap_spaceRequired(int length);

// If the key exists in the map this function returns true and the
// value will be filled in. Otherwise it returns false.
bool charactionMap_getValue(charactionMap m, char key, playeraction *value);

#endif	/* CHARACTIONMAP_H */
