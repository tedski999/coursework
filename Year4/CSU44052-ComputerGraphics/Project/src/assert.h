#ifndef WW_ASSERT_H
#define WW_ASSERT_H

#include <stdbool.h>
#include <stddef.h>

void ww_assert(const char *file, int line, const char *func, bool assertion, ...);

#ifdef WW_NO_ASSERT_DETAIL
#define ww_assert(...) ww_assert(__FILE__, __LINE__, __func__, __VA_ARGS__, NULL)
#else
#define ww_assert(...) ww_assert(__FILE__, __LINE__, __func__, __VA_ARGS__, NULL)
#endif

#endif
