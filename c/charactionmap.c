#include "charactionmap.h"

bool charactionMap_getValue(charactionMap m, char key, playeraction *value) {
  for (int i = 0; i < m.len; i++) {
    charactionPair pair = m.map[i];
    if (pair.key == key) {
      *value = pair.value;
      return true;
    }
  }
  return false;
}
