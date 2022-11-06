#include "alloc.h"
#include "assert.h"
#include <stdlib.h>

void *cbi_malloc(size_t size) {
	void *ptr = malloc(size);
	cbi_assert(ptr);
	return ptr;
}

void *cbi_calloc(size_t num, size_t size) {
	void *ptr = calloc(num, size);
	cbi_assert(ptr);
	return ptr;
}

void *cbi_realloc(void *old_ptr, size_t size) {
	void *ptr = realloc(old_ptr, size);
	cbi_assert(ptr);
	return ptr;
}

void cbi_free(void *ptr) {
	free(ptr);
}
