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

#include "controller_request.h"
#include "handlers.h"
#include "flowtable.h"
#include "../common/net.h"
#include "../common/send.h"
#include "../common/requests.h"
#include "../common/log.h"
#include "../common/config.h"
#include <stdlib.h>
#include <stdbool.h>

bool ff_flowfwd_controller_request_callback(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {
	struct ff_flowfwd_controller_request_callback_data *data = user_ptr;

	// Deal with nacks
	if (tlvs[FF_DATATYPE_ACK] && tlvs[FF_DATATYPE_ACK][0] != FF_ACK_OK) {
		ff_log(FF_LOG_WARN, "Controller couldn't find a path!");
		ff_send_ack(fd, data->src, tlvs[FF_DATATYPE_ACK][0]);
		return true;
	}

	// Is this a controller response?
	if (!tlvs[FF_DATATYPE_DSTADDR]) return false;
	if (!tlvs[FF_DATATYPE_PEERADDR]) return false;
	ff_log(FF_LOG_DBUG, "Handling controller response...");

	// Add pattern/peer pair to flowtable
	struct ff_net_addr *hop = ff_flowfwd_flowtable_get(data->user_ptr->flowtable, tlvs[FF_DATATYPE_PEERADDR]);
	ff_flowfwd_flowtable_add(data->user_ptr->flowtable, data->tlvs[FF_DATATYPE_DSTADDR], hop);
	ff_log(FF_LOG_INFO, "Set %s as forwarding route towards %s", tlvs[FF_DATATYPE_PEERADDR], tlvs[FF_DATATYPE_DSTADDR]);

	// Re-execute forwarding request with the updated flowtable
	ff_flowfwd_handle_forwarding_request(fd, data->src, requests, data->tlvs, data->user_ptr);
	return true;
}

bool ff_flowfwd_controller_request_callback_timeout(int fd, struct ff_net_addr *dst, struct ff_requests *requests, void *user_ptr) {
	struct ff_flowfwd_controller_request_callback_data *data = user_ptr;
	ff_log(FF_LOG_ERRR, "Unable to query controller!");
	ff_send_ack(fd, data->src, FF_ACK_SEND_FAILED);
	return false;
}

void ff_flowfwd_controller_request_callback_cleanup(void *user_ptr) {
	struct ff_flowfwd_controller_request_callback_data *data = user_ptr;
	ff_net_addr_destroy(data->src);
	for (int i = 0; i < ff_datatype_len; i++)
		free(data->tlvs[i]);
	free(data->tlvs);
	free(data);
}

bool ff_flowfwd_controller_advertise_callback(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {
	if (!tlvs[FF_DATATYPE_ACK]) return false;
	switch (*tlvs[FF_DATATYPE_ACK]) {
		case FF_ACK_OK: ff_log(FF_LOG_INFO, "Controller accepted peer"); break;
		default: ff_log(FF_LOG_WARN, "Controller disregard peer!"); break;
	}
	return true;
}

bool ff_flowfwd_controller_advertise_callback_timeout(int fd, struct ff_net_addr *dst, struct ff_requests *requests, void *user_ptr) {
	ff_log(FF_LOG_ERRR, "Unable to advertise peer to controller!");
	return false;
}
