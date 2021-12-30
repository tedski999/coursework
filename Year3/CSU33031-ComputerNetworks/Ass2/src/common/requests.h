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

#ifndef FF_REQUEST_H
#define FF_REQUEST_H

#include "net.h"
#include "config.h"
#include <stdbool.h>

struct ff_requests;
typedef bool (*ff_request_callback)(
	int fd, struct ff_net_addr *src, struct ff_requests *requests,
	char *tlvs[ff_datatype_len], void *user_ptr);
typedef bool (*ff_request_callback_timeout)(
	int fd, struct ff_net_addr *src, struct ff_requests *requests, void *user_ptr);
typedef void (*ff_request_callback_cleanup)(void *user_ptr);

struct ff_requests *ff_requests_create(void);
void ff_requests_send(
	int fd, struct ff_requests *requests, int timeout, struct ff_net_addr *dst, char *tlvs[ff_datatype_len],
	ff_request_callback callback, ff_request_callback_timeout callback_timeout,
	ff_request_callback_cleanup callback_cleanup, void *user_ptr);
bool ff_requests_execute_callbacks(
	int fd, struct ff_requests *requests,
	char *tlvs[ff_datatype_len], struct ff_net_addr *address);
int ff_requests_get_next_timeout(struct ff_requests *requests);
void ff_requests_abandon_expired(int fd, struct ff_requests *requests);
void ff_requests_destroy(struct ff_requests *requests);

#endif
