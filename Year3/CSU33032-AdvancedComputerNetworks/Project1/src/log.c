#include "log.h"
#include "timer.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static enum wp_log_urg verbosity;
static struct wp_timer *init_timer;

static const char *FORMAT = "%.4f [%s%s%s] %s\n";
static const char *COLORS[wp_log_urg_len] = { "\033[2;37m", "\033[0;37m", "\033[1;32m", "\033[1;33m", "\033[1;31m"};
static const char *RESET_COLOR = "\033[0m";
static const char *URG_STR[wp_log_urg_len] = { "DBUG", "INFO", "NOTE", "WARN", "ERRR" };

void wp_log_init(enum wp_log_urg urg) {
	wp_log_cleanup();
	verbosity = urg;
	init_timer = wp_timer_create();
}

void wp_log(enum wp_log_urg urg, const char *msg, ...) {
	va_list args;
	va_start(args, msg);
	wp_vlog(urg, msg, args);
	va_end(args);
}

void wp_elog(enum wp_log_urg urg, const char *msg, ...) {
	char *error = strerror(errno);
	int len = strlen(msg) + strlen(error) + 1;
	char *msg_err = malloc(len);
	strcpy(msg_err, msg);
	strcat(msg_err, error);

	va_list args;
	va_start(args, msg);
	wp_vlog(urg, msg_err, args);
	va_end(args);
}

void wp_vlog(enum wp_log_urg urg, const char *msg, va_list args) {
	if (!init_timer || urg < verbosity)
		return;

	double timing = wp_timer_measure(init_timer);

	va_list args_copy;
	va_copy(args_copy, args);
	int len = vsnprintf(NULL, 0, msg, args_copy) + 1;
	va_end(args_copy);

	char *formatted_msg = malloc(len);
	vsprintf(formatted_msg, msg, args);
	printf(FORMAT, timing, COLORS[urg], URG_STR[urg], RESET_COLOR, formatted_msg);
	free(formatted_msg);
}

void wp_log_cleanup(void) {
	if (init_timer)
		wp_timer_destroy(init_timer);
	init_timer = NULL;
}
