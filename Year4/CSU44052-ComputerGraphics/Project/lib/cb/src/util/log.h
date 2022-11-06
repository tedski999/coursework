#ifndef CBI_UTIL_LOG_H
#define CBI_UTIL_LOG_H

#include <stdarg.h>

enum cbi_log_urg {
	CB_LOG_DBUG,
	CB_LOG_INFO,
	CB_LOG_NOTE,
	CB_LOG_WARN,
	CB_LOG_ERRR,
	cbi_log_urg_len
};

#ifndef CB_NO_LOG
void cbi_log(enum cbi_log_urg urg, const char *msg, ...);
void cbi_vlog(enum cbi_log_urg urg, const char *msg, va_list args);
#else
#define cbi_log(...) (__VA_ARGS__)
#define cbi_vlog(...) (__VA_ARGS__)
#endif

#endif
