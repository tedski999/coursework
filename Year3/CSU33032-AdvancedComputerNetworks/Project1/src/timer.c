#include "timer.h"
#include <stdlib.h>
#include <time.h>

#define SEC2NSEC(sec) ((sec) * 1000000000.0)
#define NSEC2SEC(nsec) ((nsec) / 1000000000.0)

struct wp_timer { struct timespec t; };

struct wp_timer *wp_timer_create(void) {
	struct wp_timer *timer = malloc(sizeof *timer);
	clock_gettime(CLOCK_MONOTONIC, &timer->t);
	return timer;
}

double wp_timer_measure(const struct wp_timer *timer) {
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	return (now.tv_sec - timer->t.tv_sec) + NSEC2SEC(now.tv_nsec - timer->t.tv_nsec);
}

void wp_timer_set(struct wp_timer *timer, double new_time) {
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	long secs = new_time;
	long nsecs = SEC2NSEC(new_time - secs);
	timer->t.tv_sec = now.tv_sec - secs;
	timer->t.tv_nsec = now.tv_nsec - nsecs;
}

void wp_timer_destroy(struct wp_timer *timer) {
	free(timer);
}
