// Defines a type representing a direction in 2D space and some
// functions on that type.
#ifndef DIR_H
#define DIR_H

// bool
#include <stdbool.h>

// A direction in 2D space.
typedef struct {
  char x;
  char y;
} dir;

// Returns true if two directions are equal.
bool dir_equal(dir a, dir b);

// Returns true if two directions are orthogonal. Two vectors are
// orthogonal if their dot product is 0.
bool dir_orthogonal(dir a, dir b);

#endif	/* DIR_H */
