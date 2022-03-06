#include "handlers.h"
#include "../util/log.h"
#include <stdlib.h>
#include <netdb.h>
#include <pthread.h>

void wp_connection_handler(struct wp_state *state, int listening_fd) {
	wp_log(WP_DBUG, "Handling connection socket activity...");

	struct sockaddr_storage client_addr;
	socklen_t client_addrlen = sizeof client_addr;
	int client_fd = accept(listening_fd, (struct sockaddr *) &client_addr, &client_addrlen);

	int err;
	char client_host[MAX_IPADDR_LEN], client_port[MAX_PORT_LEN];
	if ((err = getnameinfo((struct sockaddr *) &client_addr, client_addrlen, client_host, sizeof client_host, client_port, sizeof client_port, NI_NUMERICHOST | NI_NUMERICSERV))) {
		wp_log(WP_ERRR, "Failed to identify client: %s", gai_strerror(err));
		return;
	}

	wp_log(WP_INFO, "Serving request from %s:%s as Client #%d.", client_host, client_port, client_fd);

	struct wp_handler_args *args = malloc(sizeof *args);
	args->state = state;
	args->fd = client_fd;

	wp_log(WP_DBUG, "Adding client to job queue...");
	pthread_mutex_lock(&state->lock);
	wp_pool_add_job(state->pool, args);
	pthread_mutex_unlock(&state->lock);
}
