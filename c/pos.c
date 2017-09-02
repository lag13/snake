#include "pos.h"

bool pos_equal(pos a, pos b) {
  return a.x == b.x && a.y == b.y;
}

bool posList_contains(posList pl, pos toFind) {
  for (int i = 0; i < pl.len; i++) {
    if (pos_equal(toFind, pl.list[i])) {
      return true;
    }
  }
  return false;
}

posList posList_append(posList pl, pos p) {
  pl.list[pl.len] = p;
  pl.len = pl.len + 1;
  return pl;
}
