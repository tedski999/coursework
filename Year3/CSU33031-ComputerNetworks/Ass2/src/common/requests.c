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

#include "requests.h"
#include "net.h"
#include "send.h"
#include "config.h"
#include "timer.h"
#include "log.h"
#include <stdlib.h>
#include <stdbool.h>

#define SEC2MSEC(sec) ((sec) * 1000.0)

struct request {
	int timeout;
	struct ff_timer *timer;
	struct ff_net_addr *address;
	ff_request_callback callback;
	ff_request_callback_timeout callback_timeout;
	ff_request_callback_cleanup callback_cleanup;
	void *user_ptr;
};

struct ff_requests {
	int count;
	struct request *items;
};

static int get_time_remaining(struct request *request) {
	return request->timeout - SEC2MSEC(ff_timer_measure(request->timer));
}

static void delete_request(struct request *items, int i) {
	if (items[i].callback_cleanup)
		items[i].callback_cleanup(items[i].user_ptr);
	ff_timer_destroy(items[i].timer);
}

struct ff_requests *ff_requests_create(void) {
	return calloc(1, sizeof (struct ff_requests));
}

void ff_requests_send(
	int fd, struct ff_requests *requests, int timeout, struct ff_net_addr *dst, char *tlvs[ff_datatype_len],
	ff_request_callback callback, ff_request_callback_timeout callback_timeout,
	ff_request_callback_cleanup callback_cleanup, void *user_ptr) {
	ff_log(FF_LOG_DBUG, "Adding new request...");
	requests->items = realloc(requests->items, sizeof *requests->items * ++requests->count);
	requests->items[requests->count - 1] = (struct request) {
		timeout, ff_timer_create(), dst, callback, callback_timeout, callback_cleanup, user_ptr
	};
	ff_send_tlvs(fd, dst, tlvs);
}

bool ff_requests_execute_callbacks(
	int fd, struct ff_requests *requests,
	char *tlvs[ff_datatype_len], struct ff_net_addr *address) {
	bool is_handled = false;
	for (int i = 0; i < requests->count; i++) {
		if (!ff_net_addr_cmp(requests->items[i].address, address)) {
			is_handled = true;
			ff_log(FF_LOG_DBUG, "Executing request callback...");
			if (!requests->items[i].callback || requests->items[i].callback(fd, address, requests, tlvs, requests->items[i].user_ptr)) {
				delete_request(requests->items, i);
				requests->items[i--] = requests->items[--requests->count];
			}
		}
	}
	return is_handled;
}

int ff_requests_get_next_timeout(struct ff_requests *requests) {
	int min_timeout = -1;
	for (int i = 0; i < requests->count; i++) {
		int remaining_time = get_time_remaining(requests->items + i);
		if (min_timeout == -1 || remaining_time < min_timeout)
			min_timeout = remaining_time;
	}
	return min_timeout;
}

void ff_requests_abandon_expired(int fd, struct ff_requests *requests) {
	for (int i = 0; i < requests->count; i++) {
		int remaining_time = get_time_remaining(requests->items + i);
		if (remaining_time <= 0) {
			ff_log(FF_LOG_WARN, "Request timed-out!");
			if (requests->items[i].callback_timeout && requests->items[i].callback_timeout(fd, requests->items[i].address, requests, requests->items[i].user_ptr)) {
				ff_timer_set(requests->items[i].timer, 0);
			} else {
				delete_request(requests->items, i);
				requests->items[i--] = requests->items[--requests->count];
			}
		}
	}
}

void ff_requests_destroy(struct ff_requests *requests) {
	for (int i = 0; i < requests->count; i++)
		delete_request(requests->items, i);
	free(requests->items);
	free(requests);
}
