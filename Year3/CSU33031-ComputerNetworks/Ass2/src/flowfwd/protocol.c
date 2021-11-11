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

#include "protocol.h"
#include "clients.h"
#include "table.h"
#include "../common/net.h"
#include "../common/log.h"
#include "../common/protocol.h"
#include "../common/config.h"
#include <stdlib.h>
#include <string.h>

static char *local_address;

void flowfwd_protocol_set_local_address(char *_local_address) {
	flowfwd_protocol_cleanup();
	local_address = strdup(_local_address);
}

enum ff_ack flowfwd_protocol_handle_send_packet(int fd, char **tlvs) {
	ff_log(FF_LOG_DBUG, "Handling send packet...");
	for (int i = 0; i < ff_data_type_len; i++)
		if (tlvs[i])
			ff_log(FF_LOG_DBUG, "  TLV: 0x%02x - %s", i, tlvs[i]);

	// Deal with missing TLVs
	if (!tlvs[FF_DATA_TYPE_MSG])
		return FF_ACK_NO_ID;
	if (!tlvs[FF_DATA_TYPE_SRC])
		return FF_ACK_NO_SRC;
	if (!tlvs[FF_DATA_TYPE_DST])
		return FF_ACK_NO_DST;
	if (!tlvs[FF_DATA_TYPE_ID])
		return FF_ACK_NO_ID;

	// If this is the destination, send data to any waiting receivers
	if (!strcmp(tlvs[FF_DATA_TYPE_DST], local_address)) {
		ff_log(FF_LOG_NOTE, "Received a packet. Relaying to any waiting clients...");
		ff_log(FF_LOG_INFO, "Source address: %s", tlvs[FF_DATA_TYPE_SRC]);
		ff_log(FF_LOG_INFO, "Contents: %s", tlvs[FF_DATA_TYPE_MSG]);
		bool is_received =  flowfwd_clients_fulfill(fd, tlvs[FF_DATA_TYPE_SRC], tlvs[FF_DATA_TYPE_MSG]);
		if (!is_received)
			ff_log(FF_LOG_WARN, "No clients were waiting to receive a message from %s!", tlvs[FF_DATA_TYPE_SRC]);
		ff_log(FF_LOG_NOTE, "TODO: At this point, this device should forward an ACK back towards %s...", tlvs[FF_DATA_TYPE_SRC]);
		return ff_ack_len;
		//return is_received
		//	? FF_ACK_OK
		//	: FF_ACK_NO_RECEIVERS;

	}

	// Otherwise, forward the message on
	else {
		ff_log(FF_LOG_NOTE, "Forwarding a packet...");

		ff_log(FF_LOG_DBUG, "Looking for next hop destination...");
		char *next_peer = flowfwd_table_get(tlvs[FF_DATA_TYPE_DST]);
		if (!next_peer) {
			ff_log(FF_LOG_INFO, "Unable to determine next hop destination.");
			// TODO: query the controller
			return FF_ACK_NOT_FOUND;
		}

		ff_log(FF_LOG_INFO, "Forwarding packet to next hop...");
		ff_log(FF_LOG_INFO, "Source address: %s", tlvs[FF_DATA_TYPE_SRC]);
		ff_log(FF_LOG_INFO, "Destination address: %s", tlvs[FF_DATA_TYPE_DST]);
		ff_log(FF_LOG_INFO, "Next hop: %s", next_peer);

		struct ff_net_address *address = ff_net_address_create(next_peer, FF_PORTNAME);
		if (!address) {
			ff_log(FF_LOG_ERRR, "Unable to resolve peer hostname %s!", next_peer);
			return FF_ACK_NOT_FOUND;
		}
		char **relayed_tlvs = calloc(ff_data_type_len, sizeof *relayed_tlvs);
		relayed_tlvs[FF_DATA_TYPE_ID] = ff_protocol_get_next_id(address);
		relayed_tlvs[FF_DATA_TYPE_MSG] = tlvs[FF_DATA_TYPE_MSG];
		relayed_tlvs[FF_DATA_TYPE_SRC] = tlvs[FF_DATA_TYPE_SRC];
		relayed_tlvs[FF_DATA_TYPE_DST] = tlvs[FF_DATA_TYPE_DST];
		bool ok = ff_protocol_send_tlvs(fd, address, relayed_tlvs);
		free(relayed_tlvs);
		ff_net_address_destroy(address);

		if (!ok) {
			ff_log(FF_LOG_ERRR, "Unable to send packet to peer %s!", next_peer);
			return FF_ACK_SEND_FAILED;
		}

		return ff_ack_len; // TODO: send back hop ack?
	}
}

enum ff_ack flowfwd_protocol_handle_client_request(int fd, char **tlvs) {
	ff_log(FF_LOG_DBUG, "Handling client packet...");
	for (int i = 0; i < ff_data_type_len; i++)
		if (tlvs[i])
			ff_log(FF_LOG_DBUG, "  TLV: 0x%02x - %s", i, tlvs[i]);

	// TODO: only allow local addresses
	// TODO: receiving timeouts

	// Deal with missing TLVs
	if (!tlvs[FF_DATA_TYPE_SRC])
		return FF_ACK_NO_SRC;
	if (!tlvs[FF_DATA_TYPE_PORT])
		return FF_ACK_NO_PORT;

	// Add to waiting clients list
	ff_log(FF_LOG_NOTE, "Adding new client at port %s waiting for a message from %s...", tlvs[FF_DATA_TYPE_PORT], tlvs[FF_DATA_TYPE_SRC]);
	flowfwd_clients_add(tlvs[FF_DATA_TYPE_SRC], tlvs[FF_DATA_TYPE_PORT]);
	return FF_ACK_OK;
}

void flowfwd_protocol_address_command(int argc, char **argv) {

	// Print the current address
	if (argc < 2 || !strcmp(argv[1], "print")) {
		if (argc > 2)
			ff_log(FF_LOG_ERRR, "Usage: %s print", argv[0]);
		else
			ff_log(FF_LOG_NOTE, "This devices address is set to '%s'.", local_address);
	}

	// Set the devices address
	else if (!strcmp(argv[1], "set")) {
		if (argc != 3) {
			ff_log(FF_LOG_ERRR, "Usage: %s set <address>", argv[0]);
		} else {
			flowfwd_protocol_set_local_address(argv[2]);
			ff_log(FF_LOG_NOTE, "Successfully set devices address to %s.", local_address);
		}
	}

	// Catch anything else
	else {
		ff_log(FF_LOG_ERRR, "Unknown %s subcommand '%s'!", argv[0], argv[1]);
	}
}

void flowfwd_protocol_cleanup(void) {
	if (local_address)
		free(local_address);
	local_address = NULL;
}
