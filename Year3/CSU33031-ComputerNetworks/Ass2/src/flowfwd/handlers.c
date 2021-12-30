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

#include "handlers.h"
#include "flowtable.h"
#include "controller_request.h"
#include "forwarding_request.h"
#include "../common/net.h"
#include "../common/requests.h"
#include "../common/config.h"
#include "../common/send.h"
#include "../common/log.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

static char **duplicate_tlvs(char *tlvs[ff_datatype_len]) {
	char **duplicate = malloc(sizeof *duplicate * ff_datatype_len);
	for (int i = 0; i < ff_datatype_len; i++)
		duplicate[i] = tlvs[i] ? strdup(tlvs[i]) : NULL;
	return duplicate;
}

// Forward packet to next hop or, if the packet is destined for here, direct towards any waiting clients
bool ff_flowfwd_handle_forwarding_request(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {
	struct ff_flowfwd_request_handler_data *data = user_ptr;
	enum ff_ack ack = FF_ACK_OK;

	// Is this a forwarding request?
	if (!tlvs[FF_DATATYPE_SRCADDR]) return false;
	if (!tlvs[FF_DATATYPE_DSTADDR]) return false;
	if (!tlvs[FF_DATATYPE_PAYLOAD]) return false;
	ff_log(FF_LOG_DBUG, "Handling forwarding request...");

	// If packet is destined to here, send payload to any clients waiting for packets from the packets source
	if (!strcmp(tlvs[FF_DATATYPE_DSTADDR], data->address)) {
		ff_log(FF_LOG_INFO, "Received packet from %s, relaying to any waiting clients...", tlvs[FF_DATATYPE_SRCADDR]);
		int clients_served = 0;
		for (int i = 0; i < data->clients_len; i++) {
			if (!strcmp(data->clients[i].srcaddr, tlvs[FF_DATATYPE_SRCADDR])) {
				ff_log(FF_LOG_DBUG, "Relaying to client...");
				ff_send_string(fd, data->clients[i].address, tlvs[FF_DATATYPE_PAYLOAD]);
				ff_net_addr_destroy(data->clients[i].address);
				free(data->clients[i].srcaddr);
				data->clients[i--] = data->clients[--data->clients_len];
				clients_served = true;
			}
		}
		if (!clients_served) {
			ff_log(FF_LOG_INFO, "No clients waiting");
			ack = FF_ACK_NO_RECEIVERS;
		}
	}

	// Otherwise, forward it to the next hop
	else {
		ff_log(FF_LOG_DBUG, "Received forwarding request from %s towards %s...", tlvs[FF_DATATYPE_SRCADDR], tlvs[FF_DATATYPE_DSTADDR]);

		// Get the next hop from flowtable. If found, forward to next hop
		struct ff_net_addr *hop = ff_flowfwd_flowtable_get(data->flowtable, tlvs[FF_DATATYPE_DSTADDR]);
		if (hop) {
			ff_log(FF_LOG_INFO, "Forwarding packet from %s towards %s via a peer...", tlvs[FF_DATATYPE_SRCADDR], tlvs[FF_DATATYPE_DSTADDR]);
			struct ff_flowfwd_forwarding_request_callback_data *callback_data = malloc(sizeof *callback_data);
			*callback_data = (struct ff_flowfwd_forwarding_request_callback_data) { ff_net_addr_dup(src), user_ptr };
			ff_requests_send(
				fd, requests, FF_REQUEST_TIMEOUT, hop, tlvs,
				ff_flowfwd_forwarding_request_callback, ff_flowfwd_forwarding_request_callback_timeout,
				ff_flowfwd_forwarding_request_callback_cleanup, callback_data);
			ack = FF_ACK_FORWARDING;
		}

		// Otherwise, query the controller and re-run this function with the updated flowtable
		else {
			ff_log(FF_LOG_INFO, "Unknown route when forwarding packet from %s towards %s. Querying controller...", tlvs[FF_DATATYPE_SRCADDR], tlvs[FF_DATATYPE_DSTADDR]);
			char *request_tlvs[ff_datatype_len] = {0};
			request_tlvs[FF_DATATYPE_SRCADDR] = data->address;
			request_tlvs[FF_DATATYPE_DSTADDR] = tlvs[FF_DATATYPE_DSTADDR];
			struct ff_flowfwd_controller_request_callback_data *callback_data = malloc(sizeof *callback_data);
			*callback_data = (struct ff_flowfwd_controller_request_callback_data) { ff_net_addr_dup(src), duplicate_tlvs(tlvs), user_ptr };
			ff_requests_send(
				fd, requests, FF_REQUEST_TIMEOUT, data->controller, request_tlvs,
				ff_flowfwd_controller_request_callback, ff_flowfwd_controller_request_callback_timeout,
				ff_flowfwd_controller_request_callback_cleanup, callback_data);
			ack = FF_ACK_ROUTING;
		}
	}

	// Send an acknowledgment back
	ff_send_ack(fd, src, ack);
	return true;
}

// Add application client to list of clients waiting for a message from the service
bool ff_flowfwd_handle_listening_request(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {
	struct ff_flowfwd_request_handler_data *data = user_ptr;

	// Is this a client listening request?
	if (!tlvs[FF_DATATYPE_SRCADDR]) return false;
	if (!tlvs[FF_DATATYPE_CLIENT]) return false;
	ff_log(FF_LOG_DBUG, "Handling client request...");

	// Add client to list of waiting clients
	if (++data->clients_len > data->clients_cap)
		data->clients = realloc(data->clients, sizeof *data->clients * (data->clients_cap *= 2));
	data->clients[data->clients_len - 1].address = ff_net_addr_create(FF_LOCALHOST, tlvs[FF_DATATYPE_CLIENT]);
	data->clients[data->clients_len - 1].srcaddr = strdup(tlvs[FF_DATATYPE_SRCADDR]);
	ff_log(FF_LOG_NOTE, "Added client :%s listening for messages from %s", tlvs[FF_DATATYPE_CLIENT], tlvs[FF_DATATYPE_SRCADDR]);

	// Acknowledge clients request
	ff_send_ack(fd, src, FF_ACK_OK);
	return true;
}

// Respond to peer address requests with the address of this device
bool ff_flowfwd_handle_address_request(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {
	struct ff_flowfwd_request_handler_data *data = user_ptr;

	// Is this a peer address request?
	if (!tlvs[FF_DATATYPE_PEERADDR]) return false;
	ff_log(FF_LOG_DBUG, "Handling peeraddr request...");

	// Respond with this devices address
	ff_log(FF_LOG_INFO, "Responding to request for address from %s...", tlvs[FF_DATATYPE_PEERADDR]);
	char *response_tlvs[ff_datatype_len] = {0};
	response_tlvs[FF_DATATYPE_PEERADDR] = data->address;
	ff_send_tlvs(fd, src, response_tlvs);

	// TODO: Should we add the source as a peer?
	//ff_flowfwd_flowtable_add(data->flowtable, response_tlvs[FF_DATATYPE_PEERADDR], src);
	return true;
}
