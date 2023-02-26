#include "alloc.h"
#include "assert.h"
#include <stdlib.h>

void *ww_malloc(size_t size) {
	void *ptr = malloc(size);
	ww_assert(ptr, "Unable to allocate %d bytes", size);
	return ptr;
}

void *ww_calloc(size_t num, size_t size) {
	void *ptr = calloc(num, size);
	ww_assert(ptr, "Unable to zero-allocate %d bytes", size);
	return ptr;
}

void *ww_realloc(void *old_ptr, size_t size) {
	void *ptr = realloc(old_ptr, size);
	ww_assert(ptr, "Unable to reallocate %d bytes", size);
	return ptr;
}

void ww_free(void *ptr) {
	free(ptr);
}
