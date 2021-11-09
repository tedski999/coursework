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

#include "peers.h"
#include "clients.h"
#include "protocol.h"
#include "../common/handler.h"
#include "../common/net.h"
#include "../common/log.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <string.h>

static void signal_handler(int a) {}

static void print_usage(FILE *output, char *execfile) {
	fprintf(output, "Usage: %s [-hv] <address> [peers...]\n\n", execfile);
	fprintf(output, "  -h --help : Display this usage message\n");
	fprintf(output, "  -v --verbosity <verbosity> : Set the output verbosity level\n\n");
	fprintf(output, "Note: The verbosity level can be set to any of [");
	for (enum ff_log_urgency urgency = FF_LOG_DBUG; urgency <= ff_log_urgency_len; urgency++)
		fprintf(output, " %s", FF_LOG_URGENCY_STR[urgency]);
	fprintf(output, " ]\n\n");
	fprintf(output, "Note: 'address' is what other other devices can address this device with.\n\n");
	fprintf(output, "Note: Only provide a peers hostname or IP address. The port is always %s.\n\n", FF_PORTNAME);
	fprintf(output, "Licensed under GPLv3\n");
	fprintf(output, "Ted Johnson 2021\n");
}
int main(int argc, char **argv) {
	int exit_code = EXIT_FAILURE;

	flowfwd_peers_init();
	flowfwd_clients_init();

	// Parse arguments
	int peers_added = 0;
	bool is_local_address_set = false;
	enum ff_log_urgency verbosity = FF_LOG_INFO;
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help") || !strcmp(argv[i], "-?")) {
			print_usage(stdout, argv[0]);
			exit_code = EXIT_SUCCESS;
			goto exit;
		} else if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbosity")) {
			if (++i >= argc) {
				fprintf(stderr, "Error: Please provide a verbosity level! Use -h for help.\n");
				goto exit;
			}
			verbosity = FF_LOG_DBUG;
			while (
				strcmp(argv[i], FF_LOG_URGENCY_STR[verbosity]) &&
				++verbosity <= ff_log_urgency_len);
			if (verbosity > ff_log_urgency_len) {
				fprintf(stderr, "Error: Unknown verbosity level '%s'! Use -h for help.\n", argv[i]);
				goto exit;
			}
		} else if (!is_local_address_set) {
			flowfwd_protocol_set_local_address(argv[i]);
			is_local_address_set = true;
		} else {
			flowfwd_peers_add(argv[i]);
			peers_added++;
		}
	}
	ff_log_init(verbosity);
	if (!is_local_address_set) {
		fprintf(stderr, "Error: Please provide a local address for other nodes to address you as! Use -h for help.\n");
		goto exit;
	}
	if (peers_added)
		ff_log(FF_LOG_INFO, "%d peer%s successfully registered from command-line arguments.", peers_added, (peers_added == 1) ? "" : "s");
	else
		ff_log(FF_LOG_WARN, "No peers were provided from command-line arguments. Either add some now or this service will only be able to receive packets.");

	// Setup signal handlers
	struct sigaction sa = {0};
	sa.sa_handler = signal_handler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	if (sigaction(SIGHUP, &sa, NULL) || sigaction(SIGINT, &sa, NULL) ||
		sigaction(SIGQUIT, &sa, NULL) || sigaction(SIGTERM, &sa, NULL)) {
		ff_elog("Failed to set signal handlers: ");
		goto exit;
	}

	// Setup command and protocol event handlers
	struct ff_command_handler command_handlers[] = {
		{ "address", flowfwd_protocol_address_command },
		{ "peers", flowfwd_peers_command },
		{ "clients", flowfwd_clients_command }
	};
	int command_handlers_len = sizeof command_handlers / sizeof *command_handlers;
	struct ff_packet_handler packet_handlers[] = {
		{ FF_DATA_TYPE_MSG, flowfwd_protocol_handle_send_packet },
		{ FF_DATA_TYPE_PORT, flowfwd_protocol_handle_client_request }
	};
	int packet_handlers_len = sizeof packet_handlers / sizeof *packet_handlers;

	// Enter event handler loop
	ff_handler(command_handlers, command_handlers_len, packet_handlers, packet_handlers_len);

	// Cleanup
	ff_log(FF_LOG_INFO, "Quitting...");
	exit_code = EXIT_SUCCESS;
exit:
	flowfwd_clients_cleanup();
	flowfwd_peers_cleanup();
	ff_log_cleanup();
	return exit_code;
}
