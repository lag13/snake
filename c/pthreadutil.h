// Defines utility functions on top of the pthread.h functions.
#ifndef PTHREADUTIL_H
#define PTHREADUTIL_H

// pthread_mutex_t 
#include <pthread.h>

// Initializes a mutex and exits with a status of 1 if that fails.
void must_pthread_mutex_init(pthread_mutex_t *restrict mutex, const pthread_mutexattr_t *restrict attr);

// Creates a thread and exits with a status of 1 if that fails.
void must_pthread_create(pthread_t *restrict thread, const pthread_attr_t *restrict attr, void *(*start_routine)(void*), void *restrict arg);

#endif	/* PTHREADUTIL_H */
