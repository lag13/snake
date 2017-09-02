// Defines utility functions on top of the stdlib.h functions.
#ifndef STDLIBUTIL_H
#define STDLIBUTIL_H

// size_t
#include <stddef.h>

// Malloc's memory and exits with a status code of 1 if the memory
// could not be allocated.
void *mustMalloc(size_t size);

#endif	/* STDLIBUTIL_H */
