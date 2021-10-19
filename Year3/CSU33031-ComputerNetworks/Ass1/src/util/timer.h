#ifndef SUBPUB_UTIL_TIMER_H
#define SUBPUB_UTIL_TIMER_H

#include <time.h>

struct subpub_timer;

struct subpub_timer *subpub_timer_create(void);
double subpub_timer_measure(const struct subpub_timer *timer);
struct timespec subpub_timer_measure_in_timespec(const struct subpub_timer *timer);
void subpub_timer_set(struct subpub_timer *timer, double new_time);
void subpub_timer_destroy(struct subpub_timer *timer);

#endif
