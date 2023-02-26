#ifndef WW_LOG_H
#define WW_LOG_H

#include <stdarg.h>

enum ww_log_urg { WW_LOG_DBUG, WW_LOG_INFO, WW_LOG_NOTE, WW_LOG_WARN, WW_LOG_ERRR, ww_log_urg_len };

#ifndef WW_NO_LOG
void ww_log(enum ww_log_urg urg, const char *msg, ...);
void ww_vlog(enum ww_log_urg urg, const char *msg, va_list args);
#else
#define ww_log(...)
#define ww_vlog(...)
#endif

#endif
