#define _GNU_SOURCE
#include "handlers.h"
#include "log.h"
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <poll.h>

#define MAX_IPADDR_LEN (16)
#define MAX_PORT_LEN (6)

struct wp_request_handler_args {
	struct wp_state *state;
	int client_fd;
};

struct http_msg {
	char *line;
	char *(*headers)[2], *body;
	int headers_len, body_len;
};

static struct http_msg *http_recv(int fd, int timeout) {
	char buf[1024];
	recv(fd, buf, 1024, 0);
	// TODO
	return NULL;
}

static void http_destroy(struct http_msg *http) {
	/*
	for (int i = 0; i < http->headers_len; i++) {
		free(http->headers[i][0]);
		free(http->headers[i][1]);
	}
	free(http->headers);
	free(http->body);
	free(http->line);
	free(http);
	*/
}

/*
static void http_cache_things() {
	wp_log(WP_DBUG, "[Client #%d -> %s:%s] Forwarding client request...", args->client_fd, host, port);
	if (send(server_fd, buf, strlen(buf), 0) <= 0) {
		wp_elog(WP_ERRR, "[Client #%d -> %s:%s] Failed to send request: ", args->client_fd, host, port);
		goto error;
	}

	wp_log(WP_DBUG, "[Client #%d <- %s:%s] Waiting for response...", args->client_fd, host, port);
	struct http_msg *response = http_recv(server_fd);

	// TODO: forward response

	close(server_fd);

	// TODO: cache the response

	http_destroy(response);
}
*/

static void maintain_connection(int client_fd, int server_fd) {

	enum { FDS_CLIENT = 0, FDS_SERVER, fds_len };
	struct pollfd fds[fds_len];
	fds[FDS_CLIENT] = (struct pollfd) { client_fd, POLLIN };
	fds[FDS_SERVER] = (struct pollfd) { server_fd, POLLIN };

	int pipes[fds_len][2];
	if (pipe(pipes[0]) || pipe(pipes[1])) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to create socket pipe: ", client_fd);
		goto error;
	}

	wp_log(WP_DBUG, "[Client #%d] Tunnel connection established.", client_fd);

	char *response = "HTTP/1.1 200 OK\r\n\r\n";
	if (send(client_fd, response, strlen(response), 0) <= 0) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to notify client: ", client_fd);
		goto error;
	}

	bool connect = true;
	while (connect) {
		if (poll(fds, fds_len, -1) > 0) {
			if (fds[FDS_CLIENT].revents & POLLIN) {
				int len = splice(fds[FDS_CLIENT].fd, NULL, pipes[FDS_CLIENT][1], NULL, CONNECTION_MSG_BUFFER_SIZE, SPLICE_F_MOVE);
				splice(pipes[FDS_CLIENT][0], NULL, fds[FDS_SERVER].fd, NULL, len, SPLICE_F_MOVE);
				wp_log(WP_DBUG, "[Client #%d] Sent %d bytes", client_fd, len);
				if (len <= 0)
					connect = false;
			}
			if (fds[FDS_SERVER].revents & POLLIN) {
				int len = splice(fds[FDS_SERVER].fd, NULL, pipes[FDS_SERVER][1], NULL, CONNECTION_MSG_BUFFER_SIZE, SPLICE_F_MOVE);
				splice(pipes[FDS_SERVER][0], NULL, fds[FDS_CLIENT].fd, NULL, len, SPLICE_F_MOVE);
				wp_log(WP_DBUG, "[Client #%d] Recv %d bytes", client_fd, len);
				if (len <= 0)
					connect = false;
			}
		} else if (errno != EINTR) {
			wp_elog(WP_ERRR, "[Client #%d] An error occurred: ", client_fd);
			connect = false;
		}
	};

error:
	wp_log(WP_DBUG, "[Client #%d] Closing tunnel connection...", client_fd);
	close(pipes[FDS_CLIENT][0]); close(pipes[FDS_CLIENT][1]);
	close(pipes[FDS_SERVER][0]); close(pipes[FDS_SERVER][1]);
}

static void *request_handler(void *raw_args) {
	struct wp_request_handler_args *args = raw_args;

	wp_log(WP_DBUG, "[Client #%d] Parsing request...", args->client_fd);

	struct http_msg *request = http_recv(args->client_fd, 10);

	// TODO: parse
	char *host = "archlinux.org", *port = "443";
	char *method = "CONNECT", *path = "/";

	wp_log(WP_INFO, "[Client #%d] %s %s:%s %s", args->client_fd, method, host, port, path);
	wp_log(WP_DBUG, "[Client #%d] Preparing request connection to %s:%s...", args->client_fd, host, port);

	int err;
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM
	};

	struct addrinfo *addr = NULL;
	if ((err = getaddrinfo(host, port, &hints, &addr))) {
		wp_log(WP_ERRR, "[Client #%d] Failed to resolve address: %s", args->client_fd, gai_strerror(err));
		goto error;
	}

	int server_fd;
	if ((server_fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) == -1) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to create request socket: ", args->client_fd);
		goto error;
	}

	char resolved_host[MAX_IPADDR_LEN], resolved_port[MAX_PORT_LEN];
	if ((err = getnameinfo(addr->ai_addr, addr->ai_addrlen, resolved_host, sizeof resolved_host, resolved_port, sizeof resolved_port, NI_NUMERICHOST | NI_NUMERICSERV))) {
		wp_log(WP_ERRR, "[Client #%d] Failed to identify destination: %s", args->client_fd, gai_strerror(err));
		goto error;
	}

	wp_log(WP_DBUG, "[Client #%d] Establishing connection to %s:%s...", args->client_fd, resolved_host, resolved_port);

	if (connect(server_fd, addr->ai_addr, addr->ai_addrlen)) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to connect: ", args->client_fd);
		goto error;
	}

	freeaddrinfo(addr);

	if (!strcmp(TUNNEL_METHOD, method)) {
		maintain_connection(args->client_fd, server_fd);
	} else {
		// TODO
	}

error:
	http_destroy(request);
	wp_log(WP_INFO, "[Client #%d] Request concluded.", args->client_fd);
	close(args->client_fd);
	close(server_fd);
	return NULL;
}

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

	wp_log(WP_INFO, "Serving %s:%s as Client #%d.", client_host, client_port, client_fd);

	// TODO: thread pool may be beneficial
	// list of threads managed in main (dynamically added when needed?)
	// here we just add a job to a queue using a lock and signal
	// waiting threads take jobs from the queue
	pthread_t thread;
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	pthread_create(&thread, &attr, request_handler, &(struct wp_request_handler_args) {
		.state = state,
		.client_fd = client_fd
	});
}

void wp_command_handler(struct wp_state *state, int input_fd) {
	wp_log(WP_DBUG, "Handling command socket activity...");

	size_t input_buffer_len;
	char *input_buffer = NULL;
	if (getline(&input_buffer, &input_buffer_len, stdin) == -1) {
		free(input_buffer);
		return;
	}
	input_buffer[strcspn(input_buffer, "\r\n")] = '\0';

	char *cmd = strtok(input_buffer, " ");
	char *arg = strtok(NULL, " ");

	pthread_mutex_lock(&state->lock);

	if (!strcmp("shutdown", cmd)) {
		if (arg) {
			wp_log(WP_ERRR, "Unexpected argument '%s'!", arg);
		} else {
			state->is_running = false;
		}
	} else if (!strcmp("block", cmd)) {
		if (!arg) {
			wp_log(WP_ERRR, "Usage: block <url>");
		} else {
			wp_log(WP_INFO, "TODO: block '%s'...", arg);
		}
	} else if (!strcmp("unblock", cmd)) {
		if (!arg) {
			wp_log(WP_ERRR, "Usage: unblock <url>");
		} else {
			wp_log(WP_INFO, "TODO: unblock '%s'...", arg);
		}
	} else if (!strcmp("cache", cmd)) {
		if (arg) {
			wp_log(WP_ERRR, "Unexpected argument '%s'!", arg);
		} else {
			wp_log(WP_INFO, "TODO: cache...");
		}
	} else {
		wp_log(WP_ERRR, "'%s' is not a valid command!", cmd);
	}

	pthread_mutex_unlock(&state->lock);
	free(input_buffer);
}
