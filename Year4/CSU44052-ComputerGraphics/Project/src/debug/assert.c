#include "assert.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

#undef cb_assert

void cb_assert(const char *file, int line, const char *func, bool assertion, ...) {
	if (!assertion) {
		printf("Assertion violation");
#ifndef CB_NO_ASSERT_DETAIL
		printf(" in %s at %s line %d\n", func, file, line);
#endif
		va_list ap;
		va_start(ap, assertion);
		char *format = va_arg(ap, char *);
		if (format) {
			vprintf(format, ap);
			printf("\n");
		}
		va_end(ap);
		exit(EXIT_FAILURE);
	}
}
