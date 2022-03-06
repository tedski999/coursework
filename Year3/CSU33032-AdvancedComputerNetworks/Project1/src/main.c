#include "handlers/handlers.h"
#include "util/hash.h"
#include "util/net.h"
#include "util/log.h"
#include "config.h"
#include "state.h"
#include <stdlib.h>
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
		goto error_a;
	}

	wp_log(WP_DBUG, "Preparing connection socket...");

	struct addrinfo *addr;
	int connection_fd = wp_open_socket(NULL, LISTENING_PORT, &addr);
	if (connection_fd == -2) {
		wp_log(WP_ERRR, "Failed to resolve local address.");
		goto error_b;
	} else if (connection_fd == -1) {
		wp_elog(WP_ERRR, "Failed to create connection socket: ");
		goto error_b;
	}

	if (setsockopt(connection_fd, SOL_SOCKET, SO_REUSEADDR, &(int){1}, sizeof (int))) {
		wp_elog(WP_ERRR, "Failed to set SO_REUSEADDR on connection socket: ");
		goto error_b;
	}

	if (bind(connection_fd, addr->ai_addr, addr->ai_addrlen)) {
		wp_elog(WP_ERRR, "Failed to bind connection socket: ");
		goto error_b;
	}

	wp_log(WP_DBUG, "Opening connection socket...");

	if (listen(connection_fd, 1)) {
		wp_elog(WP_ERRR, "Failed to open connection socket: ");
		goto error_b;
	}

	wp_log(WP_DBUG, "Starting worker thread pool...");

	struct wp_pool *pool = wp_pool_init(wp_request_handler);
	if (!pool) {
		wp_elog(WP_ERRR, "Failed to start worker thread pool: ");
		goto error_b;
	}

	wp_log(WP_NOTE, "Web proxy ready and listening on :%s", LISTENING_PORT);

	enum { FDS_TCP = 0, FDS_IPC, fds_len };
	struct pollfd fds[fds_len];
	fds[FDS_TCP] = (struct pollfd) { connection_fd, POLLIN };
	fds[FDS_IPC] = (struct pollfd) { STDIN_FILENO, POLLIN };

	struct wp_state state = {
		.lock = PTHREAD_MUTEX_INITIALIZER,
		.is_running = true,
		.pool = pool,
		.blocklist = wp_hashset_create(),
		.cache = wp_cache_create()
	};

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
	wp_log(WP_DBUG, "Cleaning up worker thread pool...");
	wp_pool_cleanup(state.pool);
	wp_log(WP_DBUG, "Cleaning up blocklist and cache...");
	wp_hashset_destroy(state.blocklist);
	wp_cache_destroy(state.cache);
error_b:
	wp_log(WP_DBUG, "Cleaning up connection socket...");
	close(connection_fd);
	freeaddrinfo(addr);
error_a:
	wp_log(WP_DBUG, "Cleaning up logger...");
	wp_log_cleanup();
	return status;
}
