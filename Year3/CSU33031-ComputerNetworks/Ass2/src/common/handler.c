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

#include "handler.h"
#include "send.h"
#include "requests.h"
#include "log.h"
#include "config.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <poll.h>

enum fds_index { FDS_UDP = 0, fds_len };

static enum ff_ack parse_tlvs(char **tlvs, char *buffer, int buffer_len) {
	while (buffer_len > 0) {

		// Deal with funky TLVs
		if (buffer[0] >= ff_datatype_len)
			return FF_ACK_INVALID_DATATYPE;
		else if (tlvs[(int) buffer[0]])
			return FF_ACK_DUPLICATE_DATATYPE;

		// Parse TLV header
		enum ff_datatype datatype = buffer[0];
		int data_len = buffer[1];
		if (data_len > buffer_len - FF_TLV_HEADER_LEN)
			return FF_ACK_INVALID_PACKET;

		// Copy TLV data
		tlvs[datatype] = malloc(data_len + 1);
		memcpy(tlvs[datatype], buffer + FF_TLV_HEADER_LEN, data_len);
		tlvs[datatype][data_len] = '\0';

		// Move on to next TLV
		int tlv_len = FF_TLV_HEADER_LEN + data_len;
		buffer += tlv_len;
		buffer_len -= tlv_len;
	}

	return ff_ack_len;
}

static void handle_udp(int fd, ff_request_handler *handlers, int handlers_len, struct ff_requests *requests, void *user_ptr) {
	ff_log(FF_LOG_DBUG, "Handling network activity...");

	// Read data from network
	struct ff_net_addr *address;
	char input_buffer[FF_MAX_PACKET_LEN + 1];
	int input_buffer_len = ff_net_recv(fd, input_buffer, FF_MAX_PACKET_LEN + 1, &address);

	// Deal with funky packets, parse TLVs and execute handlers
	char *tlvs[ff_datatype_len] = {0};
	if (input_buffer_len == -1) {
		ff_log(FF_LOG_DBUG, "Unable to receive packet!");
	} else if (input_buffer_len == -2) {
		ff_log(FF_LOG_DBUG, "Received packet is too long!");
		ff_send_ack(fd, address, FF_ACK_PACKET_TOO_LONG);
	} else if (input_buffer_len < 2) {
		ff_log(FF_LOG_DBUG, "Received packet is too short!");
		ff_send_ack(fd, address, FF_ACK_INVALID_PACKET);
	} else {
		ff_log(FF_LOG_DBUG, "Parsing packet TLVs...");
		enum ff_ack ack = parse_tlvs(tlvs, input_buffer, input_buffer_len);
		if (ack == ff_ack_len) {
			ff_log(FF_LOG_DBUG, "Checking for registered response callbacks...");
			if (!ff_requests_execute_callbacks(fd, requests, tlvs, address)) {
				ff_log(FF_LOG_DBUG, "Executing request handlers...");
				bool is_handled = false;
				for (int i = 0; i < handlers_len; i++)
					is_handled = handlers[i](fd, address, requests, tlvs, user_ptr) || is_handled;
				if (!is_handled) {
					//ff_log(FF_LOG_INFO, "Received unhandled:");
					//for (int i = 0; i < ff_datatype_len; i++)
					//	if (tlvs[i])
					//		ff_log(FF_LOG_INFO, "  0x%02x: '%d'", i, *tlvs[i]);
					//ff_send_ack(fd, address, FF_ACK_UNHANDLED);
				}
			}
		} else {
			ff_log(FF_LOG_WARN, "Received malformed packet!");
			ff_send_ack(fd, address, ack);
		}
	}

	// Cleanup
	for (int i = 0; i < ff_datatype_len; i++)
		free(tlvs[i]);
	free(address);
}

void ff_start_handler(int fd, ff_request_handler *handlers, int handlers_len, struct ff_requests *requests, void *user_ptr) {
	bool is_running = true;
	struct pollfd fds[fds_len] = { { fd, POLLIN } };
	while (is_running) {
		int timeout = ff_requests_get_next_timeout(requests);
		if (timeout >= 0)
			ff_log(FF_LOG_DBUG, "Next request timeout in %dms", timeout);
		ff_log(FF_LOG_DBUG, "Waiting for I/O events...");
		int poll_code = poll(fds, fds_len, timeout);
		if (poll_code < 0) {
			// Deal with signals and poll errors
			if (errno != EINTR)
				ff_elog("An error occurred: ");
			is_running = false;
		} else if (poll_code == 0) {
			// Deal with poll timeouts
			ff_log(FF_LOG_DBUG, "poll() timeout");
			ff_requests_abandon_expired(fd, requests);
		} else if (poll_code > 0) {
			// Deal with fd events
			ff_log(FF_LOG_DBUG, "poll() I/O activity");
			if (fds[FDS_UDP].revents & POLLIN)
				handle_udp(fds[FDS_UDP].fd, handlers, handlers_len, requests, user_ptr);
		}
	}
}
