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
