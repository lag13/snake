// Defines a type representing a position in 2D space and some
// functions on that type.
#ifndef POS_H
#define POS_H

// bool
#include <stdbool.h>
// uint8_t
#include <stdint.h>

// A position in 2D space.
typedef struct {
  uint8_t x;
  uint8_t y;
} pos;

// A list of positions.
typedef struct {
  int len;
  pos *list;
} posList;

// Returns true if two positions are equal.
bool pos_equal(pos a, pos b);

// Returns true if a list of positions contains a specific position.
bool posList_contains(posList pl, pos toFind);

// Appends one position to a list of positions. No memory allocation
// is performed and the passed in list is not modified so you must
// assign the passed in list to the result if you want it updated.
// This is reminiscent of go's append function.
posList posList_append(posList pl, pos p);

#endif	/* POS_H */
