/**
 * Subscriber Publisher Protocol
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
#include "config.h"
#include "jobs.h"
#include "protocol.h"
#include "subpubs.h"
#include "util/net.h"
#include "util/log.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <poll.h>

enum fds_index { UDP = 0, IPC, fds_len };

static struct subpub_net_address *verified_addresses = NULL;
static int verified_addresses_count = 0;

static void signal_handler(int a) {} // poll() is used to handle signals, so just ignore them here

static void print_usage(FILE *output, char *execfile) {
	fprintf(output, "Usage: %s <listening port> [verified addresses...]\n\n", execfile);
	fprintf(output, "\t-h --help : Display this usage message\n");
	fprintf(output, "\t-V --verbose <verbosity> : Set the output verbosity level\n\n");
	fprintf(output, "\t<listening port> : The port to listen on for incoming network requests\n");
	fprintf(output, "\t[verified addresses...] : Addresses to allow incoming network requests from\n\n");
	fprintf(output, "Note: Piping the output of this program or setting an invalid verbosity level\n");
	fprintf(output, "      with -v will disable all non-network data output. You can set the\n");
	fprintf(output, "      verbosity level explicitly with -v to force log output anyway.\n\n");
	fprintf(output, "Note: Verified addresses should be in the form [hostname]:[port]\n");
	fprintf(output, "      For example, 'broker:1428' will allow requests from host 'broker', port '1428'.\n");
	fprintf(output, "      Unfortunately, the current parser for this feature can't yet handle IPv6.\n\n");
	fprintf(output, "Note: The verbosity level can be set to any of [");
	for (enum subpub_log_urgency urgency = SUBPUB_LOG_DBUG; urgency < subpub_log_urgency_len; urgency++)
		fprintf(output, " %s", SUBPUB_LOG_URGENCY_STR[urgency]);
	fprintf(output, " ]\n\n");
	fprintf(output, "Licensed under GPLv3\n");
	fprintf(output, "Ted Johnson 2021\n");
}

static bool handle_poll_event(int poll_code, struct pollfd fds[fds_len]) {

	// Deal with signals and poll errors
	if (poll_code < 0) {
		if (errno != EINTR)
			subpub_elog("An error occurred: ");
		return false;
	}

	// Deal with poll timeouts
	else if (poll_code == 0) {
		subpub_log(SUBPUB_LOG_DBUG, "poll() timeout:");
		subpub_jobs_execute_expired();
		return true;
	}

	// Deal with fd events
	else {
		subpub_log(SUBPUB_LOG_DBUG, "poll() I/O activity:");
		bool ok = true;
		if (fds[UDP].revents & POLLIN) {
			subpub_log(SUBPUB_LOG_DBUG, "Handling network activity...");
			ok = subpub_handle_network(fds[UDP].fd, verified_addresses, verified_addresses_count);
		}
		if (fds[IPC].revents & POLLIN) {
			subpub_log(SUBPUB_LOG_DBUG, "Handling input command activity...");
			ok = subpub_handle_command(fds[UDP].fd, verified_addresses, verified_addresses_count);
		}
		return ok;
	}
}

int main(int argc, char **argv) {

	// Parse arguments
	char *port = NULL;
	enum subpub_log_urgency verbosity = isatty(fileno(stdout)) ? SUBPUB_LOG_INFO : subpub_log_urgency_len;
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help") || !strcmp(argv[i], "-?")) {
			print_usage(stdout, argv[0]);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbosity")) {
			if (++i >= argc) {
				fprintf(stderr, "Error: Please provide a verbosity level!\n");
				return EXIT_FAILURE;
			}
			verbosity = SUBPUB_LOG_DBUG;
			while (
				strcmp(argv[i], SUBPUB_LOG_URGENCY_STR[verbosity]) &&
				++verbosity < subpub_log_urgency_len);
		} else if (!port) {
			port = argv[i];
		} else {
			char *hostname = strsep(argv + i, ":"), *portname = argv[i];
			struct subpub_net_address new_verified_address = subpub_net_resolve(hostname, portname);
			if (!new_verified_address.addrlen) {
				fprintf(stderr, "Error: Unable to resolve verified address %s:%s!\n", hostname, portname);
				return EXIT_FAILURE;
			}
			verified_addresses = realloc(verified_addresses, sizeof *verified_addresses * ++verified_addresses_count);
			verified_addresses[verified_addresses_count - 1] = new_verified_address;
		}
	}
	if (!port) {
		fprintf(stderr, "Error: Please provide a port to listen on!\n");
		return EXIT_FAILURE;
	}

	subpub_log_init(verbosity);

	if (!verified_addresses_count) {
		subpub_log(SUBPUB_LOG_WARN, "No verified addresses provided! It's impossible to communicate between instances!");
	}

	// Setup signal handlers
	struct sigaction sa = {0};
	sa.sa_handler = signal_handler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	if (sigaction(SIGHUP, &sa, NULL) || sigaction(SIGINT, &sa, NULL) ||
		sigaction(SIGQUIT, &sa, NULL) || sigaction(SIGTERM, &sa, NULL)) {
		subpub_elog("Failed to set signal handlers: ");
		subpub_log_cleanup();
		return EXIT_FAILURE;
	}

	// Setup networking and stdin
	struct pollfd fds[fds_len];
	fds[UDP] = (struct pollfd) { subpub_net_open(subpub_net_resolve(NULL, port)), POLLIN };
	fds[IPC] = (struct pollfd) { STDIN_FILENO, POLLIN };
	if (fds[UDP].fd < 0) {
		subpub_elog("Failed to create UDP socket: ");
		subpub_log_cleanup();
		return EXIT_FAILURE;
	}

	// I/O event polling loop
	subpub_log(SUBPUB_LOG_INFO, "Ready! Listening on port %s...", port);
	while (handle_poll_event(poll(fds, fds_len, subpub_jobs_get_time_remaining()), fds));

	// Cleanup
	subpub_log(SUBPUB_LOG_INFO, "Quitting...");
	subpub_net_close(fds[UDP].fd);
	subpub_protocol_cleanup();
	subpub_subpubs_cleanup();
	subpub_jobs_cleanup();
	subpub_log_cleanup();
	free(verified_addresses);
	return EXIT_SUCCESS;
}
