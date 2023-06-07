#ifndef CB_ASSERT_H
#define CB_ASSERT_H

#include <stdbool.h>
#include <stddef.h>

void cb_assert(const char *file, int line, const char *func, bool assertion, ...);

#ifdef CB_NO_ASSERT_DETAIL
#define cb_assert(...) cb_assert(__FILE__, __LINE__, __func__, __VA_ARGS__, NULL)
#else
#define cb_assert(...) cb_assert(__FILE__, __LINE__, __func__, __VA_ARGS__, NULL)
#endif

#endif
