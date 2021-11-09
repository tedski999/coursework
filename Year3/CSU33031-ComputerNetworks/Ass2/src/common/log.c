/**
 * Flow Forwarding
 * Copyright (C) 2021 Ted Johnson TCD 19335618 <edjohnso@tcd.ie>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "log.h"
#include "timer.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static enum ff_log_urgency verbosity;
static struct ff_timer *init_timer;
static const char *FORMAT = "%.4f [%s%s%s] %s\n";
static const char *URGENCY_COLORS[ff_log_urgency_len + 1] = {
	"\033[2;37m", "\033[0;37m", "\033[1;32m", "\033[1;33m", "\033[1;31m", "\033[0m"
};
const char *FF_LOG_URGENCY_STR[ff_log_urgency_len + 1] = {
	"DBUG", "INFO", "NOTE", "WARN", "ERRR", "NONE"
};

void ff_log_init(enum ff_log_urgency _verbosity) {
	ff_log_cleanup();
	verbosity = _verbosity;
	init_timer = ff_timer_create();
}

void ff_log(enum ff_log_urgency urgency, const char *message, ...) {
	va_list args;
	va_start(args, message);
	ff_vlog(urgency, message, args);
	va_end(args);
}

void ff_elog(const char *message, ...) {
	char *error = strerror(errno);
	int message_with_error_len = strlen(message) + strlen(error) + 1;
	char *message_with_error = malloc(sizeof *message_with_error * message_with_error_len);
	strcpy(message_with_error, message);
	strcat(message_with_error, error);

	va_list args;
	va_start(args, message);
	ff_vlog(FF_LOG_ERRR, message_with_error, args);
	va_end(args);
}

void ff_vlog(enum ff_log_urgency urgency, const char *message, va_list args) {
	if (!init_timer || urgency < verbosity)
		return;

	va_list args_copy;
	va_copy(args_copy, args);
	int formatted_message_length = vsnprintf(NULL, 0, message, args_copy) + 1;
	va_end(args_copy);

	char *formatted_message = malloc(sizeof *formatted_message * formatted_message_length);
	vsprintf(formatted_message, message, args);

	double time_since_init = ff_timer_measure(init_timer);
	const char *urgency_str = FF_LOG_URGENCY_STR[urgency];
	FILE *stdfp = (urgency == FF_LOG_WARN || urgency == FF_LOG_ERRR) ? stderr : stdout;
	fprintf(
		stdfp, FORMAT, time_since_init,
		URGENCY_COLORS[urgency], urgency_str, URGENCY_COLORS[ff_log_urgency_len],
		formatted_message);
	free(formatted_message);
}

void ff_log_cleanup(void) {
	if (init_timer)
		ff_timer_destroy(init_timer);
	init_timer = NULL;
}
