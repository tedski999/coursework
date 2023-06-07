#ifndef CB_PROF_H
#define CB_PROF_H

#include <stddef.h>

enum cb_prof_event { CB_PROF_EVENT, CB_PROF_START, CB_PROF_END, cb_prof_event_len };

#ifndef CB_NO_PROF
void cb_prof(int line, const char *func, const char *file, enum cb_prof_event event, ...);
#define cb_prof(...) cb_prof(__LINE__, __func__, __FILE__, __VA_ARGS__, NULL)
void cb_prof_write(void);
#else
#define cb_prof(...)
#define cb_prof_write()
#endif

#endif
