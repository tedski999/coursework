#include "alloc.h"
#include "../debug/assert.h"
#include <stdlib.h>

void *cb_malloc(size_t size) {
	void *ptr = malloc(size);
	cb_assert(ptr);
	return ptr;
}

void *cb_calloc(size_t num, size_t size) {
	void *ptr = calloc(num, size);
	cb_assert(ptr);
	return ptr;
}

void *cb_realloc(void *old_ptr, size_t size) {
	void *ptr = realloc(old_ptr, size);
	cb_assert(ptr);
	return ptr;
}

void cb_free(void *ptr) {
	free(ptr);
}
