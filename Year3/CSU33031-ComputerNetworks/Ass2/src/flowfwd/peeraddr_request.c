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

#include "peeraddr_request.h"
#include "controller_request.h"
#include "flowtable.h"
#include "../common/net.h"
#include "../common/requests.h"
#include "../common/config.h"
#include "../common/log.h"
#include <stdlib.h>

bool ff_flowfwd_peeraddr_request_callback(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {

	// Is this a peer address response?
	if (!tlvs[FF_DATATYPE_PEERADDR]) return false;
	ff_log(FF_LOG_DBUG, "Handling peeraddr response...");

	// Add peer address/peer pair to flowtable
	struct ff_flowfwd_peeraddr_request_callback_data *data = user_ptr;
	ff_flowfwd_flowtable_add(data->user_ptr->flowtable, tlvs[FF_DATATYPE_PEERADDR], data->peer);
	ff_log(FF_LOG_INFO, "Found peer %s", tlvs[FF_DATATYPE_PEERADDR]);

	// Advertise presence to controller
	ff_log(FF_LOG_DBUG, "Advertising peer to network controller...");
	char *request_tlvs[ff_datatype_len] = {0};
	request_tlvs[FF_DATATYPE_SRCADDR] = data->user_ptr->address;
	request_tlvs[FF_DATATYPE_PEERADDR] = tlvs[FF_DATATYPE_PEERADDR];
	ff_requests_send(
		fd, requests, FF_REQUEST_TIMEOUT, data->user_ptr->controller, request_tlvs,
		ff_flowfwd_controller_advertise_callback, ff_flowfwd_controller_advertise_callback_timeout,
		NULL, NULL);
	return true;
}

bool ff_flowfwd_peeraddr_request_callback_timeout(int fd, struct ff_net_addr *dst, struct ff_requests *requests, void *user_ptr) {
	ff_log(FF_LOG_ERRR, "Unable to contact peer!");
	return false;
}

void ff_flowfwd_peeraddr_request_callback_cleanup(void *user_ptr) {
	free(user_ptr);
}
