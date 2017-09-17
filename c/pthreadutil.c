// exit()
#include <stdlib.h>
// fprintf()
// stderr
#include <stdio.h>
// strerror()
#include <string.h>

#include "pthreadutil.h"

void must_pthread_mutex_init(pthread_mutex_t *restrict mutex, const pthread_mutexattr_t *restrict attr) {
  int err = pthread_mutex_init(mutex, attr);
  if (err != 0) {
    fprintf(stderr, "mutex initialization failed: %s (%d)\n", strerror(err), err);
    exit(1);
  }
}

void must_pthread_create(pthread_t *restrict thread, const pthread_attr_t *restrict attr, void *(*start_routine)(void*), void *restrict arg) {
  int err = pthread_create(thread, attr, start_routine, arg);
  if (err != 0) {
    fprintf(stderr, "thread creation failed: %s (%d)\n", strerror(err), err);
    exit(1);
  }
}
