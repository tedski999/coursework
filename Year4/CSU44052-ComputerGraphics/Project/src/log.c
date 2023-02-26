#include "log.h"
#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

#ifndef WW_NO_LOG

static const char *LABELS[ww_log_urg_len] = { "DBUG",       "INFO",       "NOTE",       "WARN",       "ERRR"       };
static const char *COLORS[ww_log_urg_len] = { "\033[2;37m", "\033[0;37m", "\033[1;32m", "\033[1;33m", "\033[1;31m" };
static const char *COLOR_RESET = "\033[0m";

void ww_log(enum ww_log_urg urg, const char *msg, ...) {
	va_list ap;
	va_start(ap, msg);
	ww_vlog(urg, msg, ap);
	va_end(ap);
}

void ww_vlog(enum ww_log_urg urg, const char *msg, va_list ap) {
	printf("%f [%s%s%s] ", glfwGetTime(), COLORS[urg], LABELS[urg], COLOR_RESET);
	vprintf(msg, ap);
	printf("\n");
}

#endif
