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
#include "../common/net.h"
#include "../common/handler.h"
#include "../common/log.h"
#include "../common/config.h"
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

// Signals will instead be handled by poll() in the event handler
static void signal_handler(int a) {}

// Print usage
static void print_usage(char *execfile) {
	printf("Usage: %s [-h] [-v <verbosity>]\n\n", execfile);
	printf("  -h --help : Display this usage message\n");
	printf("  -v --verbosity <verbosity> : Set the output verbosity level\n\n");
	printf("Note: The verbosity level should be set to any of [");
	for (enum ff_log_urgency urgency = FF_LOG_DBUG; urgency <= ff_log_urgency_len; urgency++)
		printf(" %s", FF_LOG_URGENCY_STR[urgency]);
	printf(" ]\n\n");
	printf("Licensed under GPLv3\n");
	printf("Ted Johnson 2021\n");
}

int main(int argc, char **argv) {

	char *input_verbosity = NULL;
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help") || !strcmp(argv[i], "-?")) {
			print_usage(argv[0]);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbosity")) {
			if (++i < argc)
				input_verbosity = argv[i];
		} else {
			fprintf(stderr, "Error: Unexpected argument '%s'\n", argv[i]);
			return EXIT_FAILURE;
		}
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
		ff_flowctl_handle_advertisement,
		ff_flowctl_handle_route_request
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

	// Initialize components
	ff_log_init(verbosity);
	ff_log(FF_LOG_DBUG, "Initializing controller components...");
	struct ff_requests *requests = ff_requests_create();
	struct ff_flowctl_request_handler_data request_handler_data = {0};

	// Enter event handler loop
	ff_log(FF_LOG_NOTE, "Starting controller...");
	ff_start_handler(fd, request_handlers, request_handlers_len, requests, &request_handler_data);

	// Cleanup
	ff_log(FF_LOG_NOTE, "Quitting...");
	for (int i = 0; i < request_handler_data.services_len; i++) {
		for (int j = 0; j < request_handler_data.services[i].peers_len; j++)
			free(request_handler_data.services[i].peers[j]);
		ff_flowctl_flowtable_destroy(request_handler_data.services[i].flowtable);
		free(request_handler_data.services[i].address);
		free(request_handler_data.services[i].peers);
	}
	free(request_handler_data.services);
	ff_requests_destroy(requests);
	ff_net_close(fd);
	ff_log(FF_LOG_DBUG, "Cleanup complete");
	ff_log_cleanup();
	return EXIT_SUCCESS;
}
