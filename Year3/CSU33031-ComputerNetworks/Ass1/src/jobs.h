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

#ifndef SUBPUB_JOBS_H
#define SUBPUB_JOBS_H

#include "util/net.h"
#include <stdbool.h>

enum subpub_job_type {
	SUBPUB_EMPTY_JOB = 0,
	SUBPUB_REQUEST_JOB,
	SUBPUB_PARTIAL_JOB
};

void subpub_jobs_add(
	enum subpub_job_type type, double attempt_delay, int attempts_remaining,
	int sockfd, struct subpub_net_address address, char *buffer, int buffer_len);
int subpub_jobs_get_time_remaining(void);
void subpub_jobs_execute_expired(void);
void subpub_jobs_set_expired(char header, struct subpub_net_address address);
bool subpub_jobs_remove(char header, struct subpub_net_address address);
void subpub_jobs_cleanup(void);

#endif
