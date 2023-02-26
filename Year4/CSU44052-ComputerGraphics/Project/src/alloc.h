#ifndef WW_ALLOC_H
#define WW_ALLOC_H

#include <stddef.h>

void *ww_malloc(size_t size);
void *ww_calloc(size_t num, size_t size);
void *ww_realloc(void *ptr, size_t size);
void ww_free(void *ptr);

#endif
