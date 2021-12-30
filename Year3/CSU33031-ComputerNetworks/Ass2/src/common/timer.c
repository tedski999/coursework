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

#include "timer.h"
#include <stdlib.h>
#include <time.h>

#define SEC2NSEC(sec) ((sec) * 1000000000.0)
#define NSEC2SEC(nsec) ((nsec) / 1000000000.0)

struct ff_timer { struct timespec t; };

struct ff_timer *ff_timer_create(void) {
	struct ff_timer *timer = malloc(sizeof *timer);
	clock_gettime(CLOCK_MONOTONIC, &timer->t);
	return timer;
}

double ff_timer_measure(const struct ff_timer *timer) {
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	return (now.tv_sec - timer->t.tv_sec) + NSEC2SEC(now.tv_nsec - timer->t.tv_nsec);
}

void ff_timer_set(struct ff_timer *timer, double new_time) {
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	long secs = new_time;
	long nsecs = SEC2NSEC(new_time - secs);
	timer->t.tv_sec = now.tv_sec - secs;
	timer->t.tv_nsec = now.tv_nsec - nsecs;
}

void ff_timer_destroy(struct ff_timer *timer) {
	free(timer);
}
