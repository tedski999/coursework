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
