/**
 * Subscriber Publisher Protocol
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
extern const char *SUBPUB_LOG_URGENCY_STR[subpub_log_urgency_len];

void subpub_log_init(enum subpub_log_urgency verbosity);
void subpub_log(enum subpub_log_urgency urgency, const char *message, ...);
void subpub_elog(const char *message, ...);
void subpub_vlog(enum subpub_log_urgency urgency, const char *message, va_list args);
void subpub_log_cleanup(void);

#endif
