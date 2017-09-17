// malloc()
// exit()
#include <stdlib.h>
// fprintf()
// stderr
#include <stdio.h>
// strerror()
#include <string.h>
// errno
#include <errno.h>

#include "stdlibutil.h"

void* mustMalloc(size_t size) {
  void* ptr = malloc(size);
  if (ptr == NULL) {
    fprintf(stderr, "malloc(%zd): %s (%d)\n", size, strerror(errno), errno);
    exit(1);
  }
  return ptr;
}
