#ifndef WW_PROF_H
#define WW_PROF_H

#include <stddef.h>

enum ww_prof_event { WW_PROF_EVENT, WW_PROF_START, WW_PROF_END, ww_prof_event_len };

#ifndef WW_NO_PROF
void ww_prof(int line, const char *func, const char *file, enum ww_prof_event event, ...);
#define ww_prof(...) ww_prof(__LINE__, __func__, __FILE__, __VA_ARGS__, NULL)
#else
#define ww_prof(...)
#endif

#endif
