#include "assert.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

#undef cbi_assert

static void assert_msg(va_list args) {
	char *format = va_arg(args, char *);
	if (format) {
		vprintf(format, args);
		printf("\n");
	}
}

#ifdef CB_NO_ASSERT_DETAIL

void cbi_assert(bool assertion, ...) {
	if (!assertion) {
		printf("Assertion violation");
		va_list args;
		va_start(args, assertion);
		assert_msg(args);
		va_end(args);
		exit(EXIT_FAILURE);
	}
}

#else

void cbi_assert(const char *file, int line, const char *func, bool assertion, ...) {
	if (!assertion) {
		printf("Assertion violation in %s at %s line %d\n", func, file, line);
		va_list args;
		va_start(args, assertion);
		assert_msg(args);
		va_end(args);
		exit(EXIT_FAILURE);
	}
}

#endif
