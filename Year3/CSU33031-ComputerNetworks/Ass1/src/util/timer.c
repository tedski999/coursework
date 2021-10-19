#include "timer.h"
#include <stdlib.h>

#define SEC2NSEC(sec) ((sec) * 1000000000.0)
#define NSEC2SEC(nsec) ((nsec) / 1000000000.0)

struct subpub_timer {
	struct timespec start_time;
};

struct subpub_timer *subpub_timer_create(void) {
	struct subpub_timer *timer = malloc(sizeof *timer);
	clock_gettime(CLOCK_MONOTONIC, &timer->start_time);
	return timer;
}

double subpub_timer_measure(const struct subpub_timer *timer) {
	struct timespec diff = subpub_timer_measure_in_timespec(timer);
	return diff.tv_sec + NSEC2SEC(diff.tv_nsec);
}

struct timespec subpub_timer_measure_in_timespec(const struct subpub_timer *timer) {
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	return (struct timespec) {
		now.tv_sec - timer->start_time.tv_sec,
		now.tv_nsec - timer->start_time.tv_nsec
	};
}

void subpub_timer_set(struct subpub_timer *timer, double new_time) {
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	long secs = new_time;
	long nsecs = SEC2NSEC(new_time - secs);
	timer->start_time.tv_sec = now.tv_sec - secs;
	timer->start_time.tv_nsec = now.tv_nsec - nsecs;
}

void subpub_timer_destroy(struct subpub_timer *timer) {
	free(timer);
}
