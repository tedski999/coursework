#ifndef CB_ALLOC_H
#define CB_ALLOC_H

#include <stddef.h>

void *cb_malloc(size_t size);
void *cb_calloc(size_t num, size_t size);
void *cb_realloc(void *ptr, size_t size);
void cb_free(void *ptr);

#endif
