#ifndef SUBPUB_UTIL_LOG_H
#define SUBPUB_UTIL_LOG_H

#include <stdarg.h>

enum subpub_log_urgency {
	SUBPUB_LOG_DBUG = 0,
	SUBPUB_LOG_INFO,
	SUBPUB_LOG_NOTE,
	SUBPUB_LOG_WARN,
	SUBPUB_LOG_ERRR,
	subpub_log_urgency_len
};

void subpub_log_init(void);
void subpub_log(enum subpub_log_urgency urgency, const char *message, ...);
void subpub_elog(const char *message, ...);
void subpub_vlog(enum subpub_log_urgency urgency, const char *message, va_list args);
void subpub_log_cleanup(void);

#endif
