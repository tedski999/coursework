#ifndef CBI_UTIL_ASSERT_H
#define CBI_UTIL_ASSERT_H

#include <stdbool.h>

#ifdef CB_NO_ASSERT_DETAIL
void cbi_assert(bool assertion, ...);
#else
void cbi_assert(const char *file, int line, const char *func, bool assertion, ...);
#define cbi_assert(...) cbi_assert(__FILE__, __LINE__, __func__, __VA_ARGS__, NULL)
#endif

#endif
