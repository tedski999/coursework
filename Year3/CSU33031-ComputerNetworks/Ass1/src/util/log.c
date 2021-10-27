#include "log.h"
#include "timer.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static enum subpub_log_urgency verbosity;
static struct subpub_timer *init_timer;
static const char *FORMAT = "%.4f [%s%s%s] %s\n";
static const char *URGENCY_COLORS[subpub_log_urgency_len + 1] = {
	"\033[2;37m", "\033[0;37m", "\033[1;32m", "\033[1;33m", "\033[1;31m", "\033[0m"
};
const char *SUBPUB_LOG_URGENCY_STR[subpub_log_urgency_len] = {
	"DBUG", "INFO", "NOTE", "WARN", "ERRR"
};

void subpub_log_init(enum subpub_log_urgency _verbosity) {
	verbosity = _verbosity;
	init_timer = subpub_timer_create();
}

void subpub_log(enum subpub_log_urgency urgency, const char *message, ...) {
	va_list args;
	va_start(args, message);
	subpub_vlog(urgency, message, args);
	va_end(args);
}

void subpub_elog(const char *message, ...) {
	char *error = strerror(errno);
	int message_with_error_len = strlen(message) + strlen(error) + 1;
	char *message_with_error = malloc(sizeof *message_with_error * message_with_error_len);
	strcpy(message_with_error, message);
	strcat(message_with_error, error);

	va_list args;
	va_start(args, message);
	subpub_vlog(SUBPUB_LOG_ERRR, message_with_error, args);
	va_end(args);
}

void subpub_vlog(enum subpub_log_urgency urgency, const char *message, va_list args) {
	if (!init_timer || urgency < verbosity)
		return;

	va_list args_copy;
	va_copy(args_copy, args);
	int formatted_message_length = vsnprintf(NULL, 0, message, args_copy) + 1;
	va_end(args_copy);

	char *formatted_message = malloc(sizeof *formatted_message * formatted_message_length);
	vsprintf(formatted_message, message, args);

	double time_since_init = subpub_timer_measure(init_timer);
	const char *urgency_str = SUBPUB_LOG_URGENCY_STR[urgency];
	FILE *stdfp = (urgency == SUBPUB_LOG_WARN || urgency == SUBPUB_LOG_ERRR) ? stderr : stdout;
	fprintf(
		stdfp, FORMAT, time_since_init,
		URGENCY_COLORS[urgency], urgency_str, URGENCY_COLORS[subpub_log_urgency_len],
		formatted_message);
	free(formatted_message);
}

void subpub_log_cleanup(void) {
	if (init_timer)
		subpub_timer_destroy(init_timer);
	init_timer = NULL;
}
