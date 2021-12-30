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

#ifndef FF_HANDLER_H
#define FF_HANDLER_H

#include "send.h"
#include "requests.h"
#include "config.h"
#include <stdbool.h>

typedef bool (*ff_request_handler)(
	int fd, struct ff_net_addr *src, struct ff_requests *requests,
	char *tlvs[ff_datatype_len], void *user_ptr);

void ff_start_handler(
	int fd, ff_request_handler *handlers, int handlers_len,
	struct ff_requests *requests, void *user_ptr);

#endif
