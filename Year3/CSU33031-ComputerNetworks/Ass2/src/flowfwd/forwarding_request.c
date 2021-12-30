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

#include "forwarding_request.h"
#include "../common/net.h"
#include "../common/send.h"
#include "../common/requests.h"
#include "../common/config.h"
#include "../common/log.h"
#include <stdbool.h>

bool ff_flowfwd_forwarding_request_callback(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {

	// Is this a forwarding response?
	if (!tlvs[FF_DATATYPE_ACK]) return false;
	ff_log(FF_LOG_DBUG, "Handling forwarding response...");
	struct ff_flowfwd_forwarding_request_callback_data *data = user_ptr;

	bool delete_request = true;
	switch (*tlvs[FF_DATATYPE_ACK]) {
		case FF_ACK_OK:
			ff_log(FF_LOG_INFO, "Forwarding request successful.");
			ff_send_ack(fd, data->src, FF_ACK_OK);
			break;
		case FF_ACK_ROUTING:
			// TODO: would be nice to increase timeout here
			ff_log(FF_LOG_INFO, "Forwarding request is being routed.");
			delete_request = false;
			break;
		case FF_ACK_FORWARDING:
			// TODO: would be nice to increase timeout here
			ff_log(FF_LOG_INFO, "Forwarding request is being forwarded.");
			delete_request = false;
			break;
		default:
			ff_log(FF_LOG_WARN, "Forwarding request failed with ack %d.", *tlvs[FF_DATATYPE_ACK]);
			ff_send_ack(fd, data->src, *tlvs[FF_DATATYPE_ACK]);
			break;
	}

	return delete_request;
}

bool ff_flowfwd_forwarding_request_callback_timeout(int fd, struct ff_net_addr *dst, struct ff_requests *requests, void *user_ptr) {
	struct ff_flowfwd_forwarding_request_callback_data *data = user_ptr;
	ff_log(FF_LOG_WARN, "Failed to forward packet!");
	ff_send_ack(fd, data->src, FF_ACK_SEND_FAILED);
	return false;
}

void ff_flowfwd_forwarding_request_callback_cleanup(void *user_ptr) {
	struct ff_flowfwd_forwarding_request_callback_data *data = user_ptr;
	ff_net_addr_destroy(data->src);
}
