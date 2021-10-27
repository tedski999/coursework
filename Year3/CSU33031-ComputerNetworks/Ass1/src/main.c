#include "ipc/handler.h"
#include "ipc/fifo.h"
#include "deadline/list.h"
#include "protocol/handler.h"
#include "net/socket.h"
#include "util/log.h"
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <poll.h>

enum fds_index { UDP = 0, IPC = 1, fds_len = 2 };

static void print_usage(FILE *output, char *execfile) {
	fprintf(output, "Usage: %s <listening port>\n\n", execfile);

	fprintf(output, "\t-h --help : Display this usage message\n");
	fprintf(output, "\t-v --verbose <verbosity> : Set the output verbosity level\n");

	fprintf(output, "\nThe verbosity level can be set to any of [");
	for (enum subpub_log_urgency urgency = SUBPUB_LOG_DBUG; urgency < subpub_log_urgency_len; urgency++)
		fprintf(output, " %s", SUBPUB_LOG_URGENCY_STR[urgency]);
	fprintf(output, "]\n\n");

	fprintf(output, "Note: Piping the output of this program or setting an invalid verbosity level\n");
	fprintf(output, "      with -v will disable all non-network data output. You can set the\n");
	fprintf(output, "      verbosity level explicitly with -v to force log output anyway.\n\n");

	fprintf(output, "Licensed under GPLv3\n");
	fprintf(output, "Ted Johnson 2021\n");
}

// poll() is used to handle signals, so just ignore them here
static void signal_handler(int a) {}
static int setup_signals_handlers() {
	struct sigaction sa = {0};
	sa.sa_handler = signal_handler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	return (
		sigaction(SIGHUP, &sa, NULL) ||
		sigaction(SIGINT, &sa, NULL) ||
		sigaction(SIGQUIT, &sa, NULL) ||
		sigaction(SIGTERM, &sa, NULL)
	);
}

static bool handle_poll_event(int poll_code, struct pollfd fds[fds_len], void *deadline_list) {

	// Deal with signals and poll errors
	if (poll_code < 0) {
		if (errno != EINTR)
			subpub_elog("An error occurred: ");
		return false;
	}

	// Deal with poll timeouts
	if (poll_code == 0) {
		subpub_deadline_list_handle_expired(deadline_list);
		return true;
	}

	// Deal with fd events
	bool is_quitting = true;
	if (fds[UDP].revents & POLLIN)
		subpub_protocol_handle_incoming_data(fds[UDP].fd, deadline_list);
	if (fds[IPC].revents & POLLIN) {
		is_quitting = subpub_ipc_handle(fds[IPC].fd, fds[UDP].fd, deadline_list);
		fds[IPC].fd = subpub_ipc_fifo_clear(fds[IPC].fd, SUBPUB_IPC_PATH);
	}

	return is_quitting;
}

int main(int argc, char **argv) {

	// Parse arguments
	char *port = NULL;
	enum subpub_log_urgency verbosity = isatty(fileno(stdin)) ? SUBPUB_LOG_INFO : subpub_log_urgency_len;
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help") || !strcmp(argv[i], "-?")) {
			print_usage(stdout, argv[0]);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbosity")) {
			if (++i >= argc) {
				fprintf(stderr, "Error: Please provide a verbosity level!\n\n");
				print_usage(stderr, argv[0]);
				return EXIT_FAILURE;
			}
			verbosity = SUBPUB_LOG_DBUG;
			while (
				strcmp(argv[i], SUBPUB_LOG_URGENCY_STR[verbosity]) &&
				++verbosity < subpub_log_urgency_len);
		} else if (!port) {
			port = argv[i];
		} else {
			fprintf(stderr, "Error: Unrecognised argument '%s'!\n\n", argv[i]);
			print_usage(stderr, argv[0]);
			return EXIT_FAILURE;
		}
	}
	if (!port) {
		fprintf(stderr, "Error: Please provide a port to listen on!\n\n");
		print_usage(stderr, argv[0]);
		return EXIT_FAILURE;
	}

	subpub_log_init(verbosity);

	if (setup_signals_handlers()) {
		subpub_elog("Failed to set signal handlers: ");
		return EXIT_FAILURE;
	}

	struct subpub_net_address address = subpub_net_address_resolve(NULL, port);

	struct pollfd fds[fds_len];
	fds[UDP] = (struct pollfd) { subpub_net_socket_open(address), POLLIN };
	if (fds[UDP].fd < 0) {
		subpub_elog("Failed to create UDP socket: ");
		return EXIT_FAILURE;
	}
	fds[IPC] = (struct pollfd) { subpub_ipc_fifo_create(SUBPUB_IPC_PATH), POLLIN };
	if (fds[IPC].fd < 0) {
		subpub_elog("Failed to create IPC message queue: ");
		subpub_net_socket_close(fds[UDP].fd);
		return EXIT_FAILURE;
	}

	struct subpub_deadline_list *deadline_list = subpub_deadline_list_create();
	if (!deadline_list) {
		subpub_elog("Failed to create deadline list: ");
		subpub_net_socket_close(fds[UDP].fd);
		subpub_ipc_fifo_destroy(fds[IPC].fd, SUBPUB_IPC_PATH);
		return EXIT_FAILURE;
	}

	// I/O event polling loop
	subpub_log(SUBPUB_LOG_INFO, "Ready! Listening on port %s...", port);
	while (
		handle_poll_event(
			poll(fds, fds_len, subpub_deadline_list_get_timeout(deadline_list)),
			fds,
			deadline_list));

	subpub_log(SUBPUB_LOG_INFO, "Quitting...");
	subpub_deadline_list_destroy(deadline_list);
	subpub_net_socket_close(fds[UDP].fd);
	subpub_ipc_fifo_destroy(fds[IPC].fd, SUBPUB_IPC_PATH);
	subpub_log_cleanup();
	return EXIT_SUCCESS;
}
