#include "dir.h"

bool dir_equal(dir a, dir b) {
  return a.x == b.x && a.y == b.y;
}

bool dir_orthogonal(dir a, dir b) {
  return a.x*b.x + a.y*b.y == 0;
}
