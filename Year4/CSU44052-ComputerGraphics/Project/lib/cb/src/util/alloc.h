#ifndef CBI_UTIL_ALLOC_H
#define CBI_UTIL_ALLOC_H

#include <stddef.h>

void *cbi_malloc(size_t size);
void *cbi_calloc(size_t num, size_t size);
void *cbi_realloc(void *ptr, size_t size);
void cbi_free(void *ptr);

#endif
