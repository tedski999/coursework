#include "assert.h"
#include "log.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

#undef ww_assert

void ww_assert(const char *file, int line, const char *func, bool assertion, ...) {
	if (!assertion) {
		ww_log(WW_LOG_ERRR, "Assertion violation"
#ifndef WW_NO_ASSERT_DETAIL
			" in %s at %s line %d", func, file, line
#endif
		);
		va_list ap;
		va_start(ap, assertion);
		char *format = va_arg(ap, char *);
		if (format) ww_vlog(WW_LOG_ERRR, format, ap);
		va_end(ap);
		exit(EXIT_FAILURE);
	}
}
