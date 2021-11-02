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

#include "jobs.h"
#include "protocol.h"
#include "util/timer.h"
#include "util/log.h"
#include <stdlib.h>
#include <string.h>

#define MSECS_IN_SEC 1000
#define MAX(a,b) (((a)>(b))?(a):(b))

struct job {
	enum subpub_job_type type;
	struct subpub_timer *timer;
	double attempt_delay;
	int attempts_remaining, sockfd;
	struct subpub_net_address address;
	char *buffer;
	int buffer_len;
};

static int count, capacity;
static struct job *jobs;

static double time_remaining(struct job *job) {
	return job->attempt_delay - subpub_timer_measure(job->timer);
}

static struct job *get_next_to_expire(void) {
	struct job *next = NULL;
	for (int i = 0; i < capacity; i++)
		if (jobs[i].type)
			if (!next || time_remaining(next) > time_remaining(jobs+i))
				next = jobs+i;
	return next;
}

static void execute_job(struct job *job) {
	subpub_log(SUBPUB_LOG_DBUG, "Executing job #%d...", job - jobs);
	switch (job->type) {
		case SUBPUB_REQUEST_JOB: subpub_protocol_send(job->sockfd, job->address, job->buffer, job->buffer_len); break;
		case SUBPUB_PARTIAL_JOB: subpub_protocol_timeout_partial(job->sockfd, job->address, job->buffer, job->buffer_len); break;
		default: break;
	}
	subpub_log(SUBPUB_LOG_DBUG, "Finished executing job #%d.", job - jobs);
}

void subpub_jobs_add(
	enum subpub_job_type type, double attempt_delay, int attempts_remaining,
	int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {

	if (buffer_len <= 0)
		return;

	if (++count > capacity) {
		int new_capacity = capacity * 2;
		if (!new_capacity)
			new_capacity++;
		subpub_log(SUBPUB_LOG_DBUG, "Increasing job capacity from %d to %d...", capacity, new_capacity);
		jobs = realloc(jobs, sizeof *jobs * new_capacity);
		for (int i = capacity; i < new_capacity; i++)
			jobs[i].type = SUBPUB_EMPTY_JOB;
		capacity = new_capacity;
	}

	for (int i = 0; i < capacity; i++) {
		if (!jobs[i].type) {
			subpub_log(SUBPUB_LOG_DBUG, "Placing new job into job slot #%d...", i);
			jobs[i] = (struct job) {
				type, subpub_timer_create(),
				attempt_delay, attempts_remaining,
				sockfd, address, malloc(buffer_len), buffer_len
			};
			memcpy(jobs[i].buffer, buffer, buffer_len);
			execute_job(jobs+i);
			break;
		}
	}
}

int subpub_jobs_get_time_remaining(void) {
	subpub_log(SUBPUB_LOG_DBUG, "Finding time till next job will expire:");
	struct job *next = get_next_to_expire();
	if (!next) {
		subpub_log(SUBPUB_LOG_DBUG, "No active jobs found.");
		return -1;
	}
	double seconds = next->attempt_delay - subpub_timer_measure(next->timer);
	subpub_log(SUBPUB_LOG_DBUG, "Job slot #%d will expire in %f seconds.", next - jobs, seconds);
	return MAX(seconds, 0) * MSECS_IN_SEC;
}

void subpub_jobs_execute_expired(void) {
	struct job *job;
	while ((job = get_next_to_expire())) {
		if (subpub_timer_measure(job->timer) < job->attempt_delay)
			break;

		subpub_log(SUBPUB_LOG_DBUG, "Job slot #%d has expired.", job - jobs);
		subpub_log(SUBPUB_LOG_WARN, "Command 0x%02x timed-out.", (unsigned char) job->buffer[0]);
		if (job->attempts_remaining-- <= 0) {
			subpub_log(SUBPUB_LOG_ERRR, "Abandoned command 0x%02x.", (unsigned char) job->buffer[0]);
			if (!subpub_jobs_remove(job->buffer[0], job->address))
				subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) job->buffer[0]);
			continue;
		} else {
			subpub_log(SUBPUB_LOG_WARN, "Re-executing command 0x%02x...", (unsigned char) job->buffer[0]);
			execute_job(job);
			subpub_timer_set(job->timer, 0);
		}
	}
}

void subpub_jobs_set_expired(char header, struct subpub_net_address address) {
	for (int i = 0; i < capacity; i++) {
		if (jobs[i].type && jobs[i].buffer[0] == header && !subpub_net_address_cmp(jobs[i].address, address)) {
			subpub_log(SUBPUB_LOG_DBUG, "Marking job slot #%d as expired...", i);
			subpub_timer_set(jobs[i].timer, jobs[i].attempt_delay);
		}
	}
}

bool subpub_jobs_remove(char header, struct subpub_net_address address) {
	bool found = false;
	for (int i = 0; i < capacity; i++) {
		if (jobs[i].type && jobs[i].buffer[0] == header && !subpub_net_address_cmp(jobs[i].address, address)) {
			subpub_log(SUBPUB_LOG_DBUG, "Removing job from job slot #%d...", i);
			subpub_timer_destroy(jobs[i].timer);
			free(jobs[i].buffer);
			jobs[i].type = SUBPUB_EMPTY_JOB;
			count--;
			found = true;
		}
	}
	return found;
}

void subpub_jobs_cleanup(void) {
	for (int i = 0; i < capacity; i++) {
		if (jobs[i].type) {
			subpub_timer_destroy(jobs[i].timer);
			free(jobs[i].buffer);
			jobs[i].type = SUBPUB_EMPTY_JOB;
		}
	}
	free(jobs);
	jobs = NULL;
	count = 0;
}
