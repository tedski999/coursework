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

#ifndef FF_FLOWFWD_FORWARDING_REQUEST_H
#define FF_FLOWFWD_FORWARDING_REQUEST_H

#include "handlers.h"
#include "../common/net.h"
#include "../common/requests.h"
#include "../common/config.h"
#include <stdbool.h>

struct ff_flowfwd_forwarding_request_callback_data {
	struct ff_net_addr *src;
	struct ff_flowfwd_request_handler_data *user_ptr;
};

bool ff_flowfwd_forwarding_request_callback(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr);
bool ff_flowfwd_forwarding_request_callback_timeout(int fd, struct ff_net_addr *dst, struct ff_requests *requests, void *user_ptr);
void ff_flowfwd_forwarding_request_callback_cleanup(void *user_ptr);

#endif
