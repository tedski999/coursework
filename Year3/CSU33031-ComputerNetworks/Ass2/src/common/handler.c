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
#include "protocol.h"
#include "log.h"
#include "net.h"
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <poll.h>

enum fds_index { FDS_UDP = 0, FDS_IPC, fds_len };

static enum ff_ack parse_tlvs(char **tlv_ptrs, char *buffer, int buffer_len) {
	while (buffer_len > 0) {

		// Deal with funky TLVs
		if (buffer[0] >= ff_data_type_len)
			return FF_ACK_INVALID_DATA_TYPE;
		else if (tlv_ptrs[(int) buffer[0]])
			return FF_ACK_DUPLICATE_DATA_TYPE;

		// Parse TLV header
		enum ff_data_type data_type = buffer[0];
		int data_len = buffer[1];
		if (data_len > buffer_len - FF_TLV_HEADER_LEN)
			return FF_ACK_INVALID_PACKET;

		// Copy TLV data
		tlv_ptrs[data_type] = malloc(data_len + 1);
		memcpy(tlv_ptrs[data_type], buffer + FF_TLV_HEADER_LEN, data_len);
		tlv_ptrs[data_type][data_len] = '\0';

		// Move on to next TLV
		int tlv_len = FF_TLV_HEADER_LEN + data_len;
		buffer += tlv_len;
		buffer_len -= tlv_len;
	}

	return ff_ack_len;
}

static void handle_udp(int fd, struct ff_packet_handler *packet_handlers, int packet_handlers_len) {
	ff_log(FF_LOG_DBUG, "Handling network activity...");

	// Read data from network
	struct ff_net_address *address;
	char input_buffer[FF_MAX_PACKET_LEN + 1];
	int input_buffer_len = ff_net_recv(fd, input_buffer, FF_MAX_PACKET_LEN + 1, &address);

	// Deal with funky packets
	if (input_buffer_len == -1) {
		return;
	} else if (input_buffer_len == -2) {
		ff_protocol_send_ack(fd, address, FF_ACK_PACKET_TOO_LONG);
		return;
	} else if (input_buffer_len < 2) {
		ff_protocol_send_ack(fd, address, FF_ACK_INVALID_PACKET);
		return;
	}

	// Attempt to parse TLVs from packet
	ff_log(FF_LOG_DBUG, "Parsing packet TLVs...");
	char **tlv_ptrs = calloc(ff_data_type_len, sizeof *tlv_ptrs);
	enum ff_ack ack = parse_tlvs(tlv_ptrs, input_buffer, input_buffer_len);

	// Send an error back if TLVs couldn't be parsed
	if (ack != ff_ack_len) {
		ff_protocol_send_ack(fd, address, ack);
	}

	// Otherwise, execute appropriate packet handlers for each TLV parsed
	else {

		ff_log(FF_LOG_DBUG, "Executing TLV handlers...");
		bool is_handled = false;
		for (int i = 0; i < packet_handlers_len; i++) {
			if (tlv_ptrs[packet_handlers[i].type]) {
				ff_log(FF_LOG_DBUG, "Executing packet handler #%i...", i);
				is_handled = true;
				ack = packet_handlers[i].callback(fd, tlv_ptrs);
				if (ack != ff_ack_len)
					ff_protocol_send_ack(fd, address, ack);
			}
		}

		// Send an ack if none of the TLVs triggered a handler
		if (!is_handled)
			ff_protocol_send_ack(fd, address, FF_ACK_UNHANDLED);
	}

	// Cleanup parsed TLVs
	for (int i = 0; i < ff_data_type_len; i++)
		if (tlv_ptrs[i])
			free(tlv_ptrs[i]);
	free(tlv_ptrs);
}

static bool handle_ipc(int fd, struct ff_command_handler *command_handlers, int command_handlers_len) {
	ff_log(FF_LOG_DBUG, "Handling input activity...");

	// Read command from stdin
	// TODO: should use fd instead of stdin
	size_t input_buffer_len;
	char *input_buffer = NULL;
	if (getline(&input_buffer, &input_buffer_len, stdin) == -1) {
		free(input_buffer);
		return true;
	}
	input_buffer[strcspn(input_buffer, "\n")] = '\0';

	// Parse input
	ff_log(FF_LOG_DBUG, "Parsing command...");
	int command_len = 0;
	char **command = NULL;
	char *separators = " ";
	char *token = strtok(input_buffer, separators);
	while (token) {
		command = realloc(command, sizeof *command * ++command_len);
		command[command_len - 1] = token;
		token = strtok(NULL, separators);
	}

	// Execute appropriate handler
	ff_log(FF_LOG_DBUG, "Executing command handlers...");
	bool is_quitting, command_found;
	is_quitting = command_found = !strcmp(command[0], "quit");
	for (int i = 0; !command_found && i < command_handlers_len; i++) {
		if (!strcmp(command[0], command_handlers[i].label)) {
			ff_log(FF_LOG_DBUG, "Executing %s command...", command[0]);
			command_handlers[i].callback(command_len, command);
			command_found = true;
		}
	}

	if (!command_found)
		ff_log(FF_LOG_ERRR, "Command '%s' does not exist!", command[0]);
	free(command);
	return !is_quitting;
}

void ff_handler(
	struct ff_command_handler *command_handlers, int command_handlers_len,
	struct ff_packet_handler *packet_handlers, int packet_handlers_len) {

	// Setup udp and ipc
	struct pollfd fds[fds_len];
	struct ff_net_address *localhost = ff_net_address_create(NULL, FF_PORTNAME);
	if (!localhost) {
		ff_log(FF_LOG_ERRR, "Failed to create UDP listening port: Invalid localhost port");
		return;
	}
	fds[FDS_UDP] = (struct pollfd) { ff_net_open(localhost), POLLIN };
	fds[FDS_IPC] = (struct pollfd) { STDIN_FILENO, POLLIN };
	ff_net_address_destroy(localhost);
	if (fds[FDS_UDP].fd < 0) {
		ff_elog("Failed to create UDP listening socket: ");
		return;
	}

	// Main loop
	ff_log(FF_LOG_INFO, "Ready! Listening on port %s...", FF_PORTNAME);
	bool is_running = true;
	while (is_running) {

		ff_log(FF_LOG_DBUG, "Waiting for I/O events...");
		int poll_code = poll(fds, fds_len, -1);

		// Deal with signals and poll errors
		if (poll_code < 0) {
			if (errno != EINTR)
				ff_elog("An error occurred: ");
			is_running = false;
		}

		// Deal with poll timeouts
		else if (poll_code == 0) {
			ff_log(FF_LOG_DBUG, "poll() timeout:");
			//TODO: ff_jobs_execute_expired();
		}

		// Deal with fd events
		else {
			ff_log(FF_LOG_DBUG, "poll() I/O activity:");
			if (fds[FDS_UDP].revents & POLLIN)
				handle_udp(fds[FDS_UDP].fd, packet_handlers, packet_handlers_len);
			if (fds[FDS_IPC].revents & POLLIN)
				is_running = handle_ipc(fds[FDS_IPC].fd, command_handlers, command_handlers_len);
		}
	}

	// Cleanup
	ff_log(FF_LOG_DBUG, "Exited event handler loop.");
	ff_net_close(fds[FDS_UDP].fd);
}
