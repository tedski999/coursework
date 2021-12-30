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
#include "peeraddr_request.h"
#include "../common/net.h"
#include "../common/log.h"
#include "../common/requests.h"
#include "../common/handler.h"
#include "../common/config.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

#define INITIAL_CLIENTS_CAP 4

// Signals will instead be handled by poll() in the event handler
static void signal_handler(int a) {}

// Print usage
static void print_usage(char *execfile) {
	printf("Usage: %s [-h] [-v <verbosity>] <address> <controller> [peers]\n\n", execfile);
	printf("  -h --help : Display this usage message\n");
	printf("  -v --verbosity <verbosity> : Set the output verbosity level\n\n");
	printf("Note: The verbosity level should be set to any of [");
	for (enum ff_log_urgency urgency = FF_LOG_DBUG; urgency <= ff_log_urgency_len; urgency++)
		printf(" %s", FF_LOG_URGENCY_STR[urgency]);
	printf(" ]\n");
	printf("Note: A devices address is what other other devices can address this device with.\n");
	printf("Note: Only provide a controllers or peers hostname or IP address. The port is always %s.\n\n", FF_PORTNAME);
	printf("Licensed under GPLv3\n");
	printf("Ted Johnson 2021\n");
}

int main(int argc, char **argv) {

	// Read arguments
	char *address = NULL;
	char *input_controller = NULL;
	char *input_peers = NULL;
	char *input_verbosity = NULL;
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help") || !strcmp(argv[i], "-?")) {
			print_usage(argv[0]);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbosity")) {
			if (++i < argc)
				input_verbosity = argv[i];
		} else if (!address) {
			address = argv[i];
		} else if (!input_controller) {
			input_controller = argv[i];
		} else if (!input_peers) {
			input_peers = argv[i];
		} else {
			fprintf(stderr, "Error: Unexpected argument '%s'\n", argv[i]);
			return EXIT_FAILURE;
		}
	}
	if (!address) {
		fprintf(stderr, "Error: No address provided\n");
		return EXIT_FAILURE;
	}

	// Setup UDP socket
	struct ff_net_addr *localhost = ff_net_addr_create(NULL, FF_PORTNAME);
	if (!localhost) {
		fprintf(stderr, "Failed to create UDP listening port: Invalid localhost port\n");
		return 1;
	}
	int fd = ff_net_open(localhost);
	ff_net_addr_destroy(localhost);
	if (fd < 0) {
		perror("Failed to create UDP listening socket: ");
		return 1;
	}

	// Allow time for all docker containers to start
	sleep(1);

	// Set request handlers
	ff_request_handler request_handlers[] = {
		ff_flowfwd_handle_forwarding_request,
		ff_flowfwd_handle_listening_request,
		ff_flowfwd_handle_address_request
	};
	int request_handlers_len = sizeof request_handlers / sizeof *request_handlers;

	// Set signal handlers
	struct sigaction sa = {0};
	sa.sa_handler = signal_handler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	if (sigaction(SIGHUP, &sa, NULL) || sigaction(SIGINT, &sa, NULL) ||
		sigaction(SIGQUIT, &sa, NULL) || sigaction(SIGTERM, &sa, NULL)) {
		perror("Error: Failed to set signal handlers");
		return EXIT_FAILURE;
	}

	// Parse log verbosity
	enum ff_log_urgency verbosity = FF_LOG_INFO;
	if (input_verbosity)
		for (verbosity = FF_LOG_DBUG; verbosity < ff_log_urgency_len; verbosity++)
			if (!strcmp(input_verbosity, FF_LOG_URGENCY_STR[verbosity]))
				break;

	// Resolve peers
	int peers_len = 0;
	struct ff_net_addr **peers = NULL;
	if (input_peers) {
		char *sep = ",";
		char *token = strtok(input_peers, sep);
		while (token) {
			peers = realloc(peers, sizeof *peers * ++peers_len);
			peers[peers_len - 1] = ff_net_addr_create(token, FF_PORTNAME);
			if (!peers[peers_len - 1]) {
				for (int i = 0; i < peers_len - 1; i++)
					ff_net_addr_destroy(peers[i]);
				free(peers);
				fprintf(stderr, "Unable to resolve peer address '%s'\n", token);
				return EXIT_FAILURE;
			}
			token = strtok(NULL, sep);
		}
	}

	// Resolve controller
	struct ff_net_addr *controller = ff_net_addr_create(input_controller, FF_PORTNAME);
	if (!controller) {
		for (int i = 0; i < peers_len; i++)
			ff_net_addr_destroy(peers[i]);
		free(peers);
		fprintf(stderr, "Unable to resolve controller address '%s'\n", input_controller);
		return EXIT_FAILURE;
	}

	// Initialize components
	ff_log_init(verbosity);
	ff_log(FF_LOG_DBUG, "Initializing service components...");
	struct ff_requests *requests = ff_requests_create();
	struct ff_flowfwd_request_handler_data request_handler_data = {
		address, controller, ff_flowfwd_flowtable_create(), 0, INITIAL_CLIENTS_CAP,
		malloc(sizeof *request_handler_data.clients * INITIAL_CLIENTS_CAP)
	};

	// Request the address of every peer
	ff_log(FF_LOG_DBUG, "Fetching addresses from %d peers...", peers_len);
	char *peeraddr_request_tlvs[ff_datatype_len] = {0};
	peeraddr_request_tlvs[FF_DATATYPE_PEERADDR] = address;
	for (int i = 0; i < peers_len; i++) {
		struct ff_flowfwd_peeraddr_request_callback_data *callback_data = malloc(sizeof *callback_data);
		*callback_data = (struct ff_flowfwd_peeraddr_request_callback_data) { peers[i], &request_handler_data };
		ff_requests_send(
			fd, requests, FF_REQUEST_TIMEOUT, peers[i], peeraddr_request_tlvs,
			ff_flowfwd_peeraddr_request_callback, ff_flowfwd_peeraddr_request_callback_timeout,
			ff_flowfwd_peeraddr_request_callback_cleanup, callback_data);
	}

	// Enter event handler loop
	ff_log(FF_LOG_NOTE, "Starting forwarding service with address %s:%s...", address, FF_PORTNAME);
	ff_start_handler(fd, request_handlers, request_handlers_len, requests, &request_handler_data);

	// Cleanup
	ff_log(FF_LOG_NOTE, "Quitting...");
	for (int i = 0; i < peers_len; i++)
		ff_net_addr_destroy(peers[i]);
	free(peers);
	for (int i = 0; i < request_handler_data.clients_len; i++) {
		ff_net_addr_destroy(request_handler_data.clients[i].address);
		free(request_handler_data.clients[i].srcaddr);
	}
	free(request_handler_data.clients);
	ff_flowfwd_flowtable_destroy(request_handler_data.flowtable);
	ff_net_addr_destroy(request_handler_data.controller);
	ff_requests_destroy(requests);
	ff_net_close(fd);
	ff_log(FF_LOG_DBUG, "Cleanup complete");
	ff_log_cleanup();
	return EXIT_SUCCESS;
}
