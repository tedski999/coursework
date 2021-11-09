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

#ifndef FF_LOG_H
#define FF_LOG_H

#include <stdarg.h>

enum ff_log_urgency {
	FF_LOG_DBUG = 0,
	FF_LOG_INFO,
	FF_LOG_NOTE,
	FF_LOG_WARN,
	FF_LOG_ERRR,
	ff_log_urgency_len
};
extern const char *FF_LOG_URGENCY_STR[ff_log_urgency_len + 1];

void ff_log_init(enum ff_log_urgency verbosity);
void ff_log(enum ff_log_urgency urgency, const char *message, ...);
void ff_elog(const char *message, ...);
void ff_vlog(enum ff_log_urgency urgency, const char *message, va_list args);
void ff_log_cleanup(void);

#endif
