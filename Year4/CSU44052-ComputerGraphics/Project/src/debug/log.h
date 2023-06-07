#ifndef CB_LOG_H
#define CB_LOG_H

#include <stdarg.h>

enum cb_log_urg { CB_LOG_DBUG, CB_LOG_INFO, CB_LOG_NOTE, CB_LOG_WARN, CB_LOG_ERRR, cb_log_urg_len };

#ifndef CB_NO_LOG
void cb_log(enum cb_log_urg urg, const char *msg, ...);
void cb_vlog(enum cb_log_urg urg, const char *msg, va_list args);
#else
#define cb_log(...)
#define cb_vlog(...)
#endif

#endif
