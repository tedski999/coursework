#include "handlers.h"
#include "log.h"
#include "config.h"
#include <stdlib.h>
#include <stdbool.h>
#include <netdb.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <poll.h>

static void signal_handler(int sig) {}

int main(int argc, char **argv) {

	int status = EXIT_FAILURE;
	wp_log_init(WP_DBUG);

	wp_log(WP_DBUG, "Registering signal handlers...");

	struct sigaction sa = {0};
	sa.sa_handler = signal_handler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	if (sigaction(SIGHUP, &sa, NULL) || sigaction(SIGINT, &sa, NULL) ||
		sigaction(SIGQUIT, &sa, NULL) || sigaction(SIGTERM, &sa, NULL)) {
		wp_elog(WP_ERRR, "Failed to set signal handlers: ");
		goto error_early;
	}

	wp_log(WP_DBUG, "Preparing connection socket...");

	int err;
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM,
		.ai_flags = AI_PASSIVE
	};

	struct addrinfo *addr = NULL;
	if ((err = getaddrinfo(NULL, LISTENING_PORT, &hints, &addr))) {
		wp_log(WP_ERRR, "Failed to resolve local address: %s", gai_strerror(err));
		goto error_early;
	}

	int connection_fd;
	if ((connection_fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) == -1) {
		wp_elog(WP_ERRR, "Failed to create connection socket: ");
		goto error_early;
	}

	if (setsockopt(connection_fd, SOL_SOCKET, SO_REUSEADDR, &(int){1}, sizeof (int))) {
		wp_elog(WP_ERRR, "Failed to set SO_REUSEADDR on connection socket: ");
		goto error;
	}

	if (bind(connection_fd, addr->ai_addr, addr->ai_addrlen)) {
		wp_elog(WP_ERRR, "Failed to bind connection socket: ");
		goto error;
	}

	freeaddrinfo(addr);

	wp_log(WP_DBUG, "Opening connection socket...");

	if (listen(connection_fd, 1)) {
		wp_elog(WP_ERRR, "Failed to open connection socket: ");
		goto error;
	}

	enum { FDS_TCP = 0, FDS_IPC, fds_len };
	struct pollfd fds[fds_len];
	fds[FDS_TCP] = (struct pollfd) { connection_fd, POLLIN };
	fds[FDS_IPC] = (struct pollfd) { STDIN_FILENO, POLLIN };

	struct wp_state state = {
		.lock = PTHREAD_MUTEX_INITIALIZER,
		.is_running = true,
		.blocklist = NULL,
		.cache = NULL
	};

	wp_log(WP_NOTE, "Web proxy ready and listening on :%s", LISTENING_PORT);

	// TODO: not sure about POLLIN
	while (state.is_running) {
		if (poll(fds, fds_len, -1) > 0) {
			if (fds[FDS_TCP].revents & POLLIN)
				wp_connection_handler(&state, fds[FDS_TCP].fd);
			if (fds[FDS_IPC].revents & POLLIN)
				wp_command_handler(&state, fds[FDS_IPC].fd);
		} else {
			if (errno != EINTR)
				wp_elog(WP_ERRR, "An error occurred: ");
			state.is_running = false;
		}
	}

	status = EXIT_SUCCESS;
	wp_log(WP_NOTE, "Exiting...");
error:
	close(connection_fd);
error_early:
	wp_log_cleanup();
	return status;
}
