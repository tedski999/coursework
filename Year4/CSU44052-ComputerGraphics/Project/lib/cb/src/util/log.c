#include "log.h"
#include "../cfg.h"
#include <stdio.h>
#include <stdarg.h>
#include <time.h>

#ifndef CB_NO_LOG

static const char *FORMAT = "%0.2d:%0.2d:%0.2d [%s%s%s] ";
static const char *LABELS[cbi_log_urg_len] = { "DBUG",       "INFO",       "NOTE",       "WARN",       "ERRR"       };
static const char *COLORS[cbi_log_urg_len] = { "\033[2;37m", "\033[0;37m", "\033[1;32m", "\033[1;33m", "\033[1;31m" };
static const char *COLOR_RESET = "\033[0m";

void cbi_log(enum cbi_log_urg urg, const char *msg, ...) {
	va_list args;
	va_start(args, msg);
	cbi_vlog(urg, msg, args);
	va_end(args);
}

void cbi_vlog(enum cbi_log_urg urg, const char *msg, va_list args) {
	if (urg < CB_LOG_VERBOSITY)
		return;
	time_t t = time(NULL);
	struct tm *lt = localtime(&t);
	printf(FORMAT, lt->tm_hour, lt->tm_min, lt->tm_sec, COLORS[urg], LABELS[urg], COLOR_RESET);
	vprintf(msg, args);
	printf("\n");
}

#endif
