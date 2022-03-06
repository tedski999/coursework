#ifndef WP_UTIL_LOG_H
#define WP_UTIL_LOG_H

#include <stdarg.h>

enum wp_log_urg { WP_DBUG = 0, WP_INFO, WP_NOTE, WP_WARN, WP_ERRR, wp_log_urg_len };

void wp_log_init(enum wp_log_urg verbosity);
void wp_log(enum wp_log_urg urg, const char *msg, ...);
void wp_elog(enum wp_log_urg urg, const char *msg, ...);
void wp_vlog(enum wp_log_urg urg, const char *msg, va_list args);
void wp_log_cleanup(void);

#endif
