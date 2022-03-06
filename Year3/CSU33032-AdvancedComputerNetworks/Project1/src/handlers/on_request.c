#define _GNU_SOURCE
#include "handlers.h"
#include "../config.h"
#include "../util/net.h"
#include "../util/log.h"
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <poll.h>
#include <fcntl.h>
#include <curl/curl.h>

static int connect_to_remote(int client_fd, char *host, char *port) {
	wp_log(WP_DBUG, "[Client #%d] Preparing request connection to %s:%s...", client_fd, host, port);

	struct addrinfo *addr;
	int server_fd = wp_open_socket(host, port, &addr);
	if (server_fd == -2) {
		wp_log(WP_ERRR, "[Client #%d] Failed to resolve address.", client_fd);
		return -1;
	} else if (server_fd == -1) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to create request socket: ", client_fd);
		return -1;
	}

	char resolved_host[MAX_IPADDR_LEN], resolved_port[MAX_PORT_LEN];
	int err = getnameinfo(addr->ai_addr, addr->ai_addrlen, resolved_host, MAX_IPADDR_LEN, resolved_port, MAX_PORT_LEN, NI_NUMERICHOST | NI_NUMERICSERV);
	if (err) {
		wp_log(WP_ERRR, "[Client #%d] Failed to identify destination: %s", client_fd, gai_strerror(err));
		return -1;
	}

	wp_log(WP_DBUG, "[Client #%d] Establishing connection to %s:%s...", client_fd, resolved_host, resolved_port);

	if (connect(server_fd, addr->ai_addr, addr->ai_addrlen)) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to connect: ", client_fd);
		return -1;
	}

	freeaddrinfo(addr);
	return server_fd;
}

static void handle_http_request(struct wp_state *state, int client_fd, char *host, char *port, struct wp_http_msg *request) {

	pthread_mutex_lock(&state->lock);
	char *response_data = wp_cache_get(state->cache, request);
	pthread_mutex_unlock(&state->lock);

	if (!response_data) {

		int server_fd = connect_to_remote(client_fd, host, port);
		if (server_fd < 0)
			return;

		wp_log(WP_DBUG, "[Client #%d] Forwarding request to server...", client_fd);
		request->headers_len += 1;
		request->headers = realloc(request->headers, sizeof *request->headers * request->headers_len);
		request->headers[request->headers_len - 1] = HTTP_VIA_HEADER;
		char *request_data = wp_http_to_string(request);
		int err = send(server_fd, request_data, strlen(request_data), 0);
		free(request_data);
		if (err <= 0) {
			close(server_fd);
			wp_elog(WP_ERRR, "[Client #%d] Failed to forward request: ", client_fd);
			goto error;
		}

		wp_log(WP_DBUG, "[Client #%d] Waiting for response from server...", client_fd);
		struct wp_http_msg *response = wp_http_recv(server_fd, RECV_TIMEOUT);
		close(server_fd);
		if (!response) {
			wp_elog(WP_ERRR, "[Client #%d] No response received: ", client_fd);
			goto error;
		}

		response->headers_len += 1;
		response->headers = realloc(response->headers, sizeof *response->headers * response->headers_len);
		response->headers[response->headers_len - 1] = HTTP_VIA_HEADER;
		response_data = wp_http_to_string(response);

		pthread_mutex_lock(&state->lock);
		if (wp_cache_add(state->cache, request, response)) {
			wp_log(WP_DBUG, "[Client #%d] Response saved to cache.", client_fd);
		} else {
			wp_log(WP_DBUG, "[Client #%d] Response not suitable for caching.", client_fd);
			wp_http_destroy(response);
		}
		pthread_mutex_unlock(&state->lock);

	} else {
		wp_log(WP_INFO, "[Client #%d] Valid response found in cache.", client_fd);
	}

	wp_log(WP_DBUG, "[Client #%d] Forwarding response to client...", client_fd);
	if (send(client_fd, response_data, strlen(response_data), 0) <= 0) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to forward response: ", client_fd);
		goto error;
	}

error:
	free(response_data);
}

static void handle_tunnel_request(struct wp_state *state, int client_fd, char *host, char *port) {

	int server_fd = connect_to_remote(client_fd, host, port);
	if (server_fd < 0)
		return;

	enum { FDS_CLIENT = 0, FDS_SERVER, fds_len };
	struct pollfd fds[fds_len];
	fds[FDS_CLIENT] = (struct pollfd) { client_fd, POLLIN };
	fds[FDS_SERVER] = (struct pollfd) { server_fd, POLLIN };

	int pipes[fds_len][2];
	if (pipe(pipes[0]) || pipe(pipes[1])) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to create socket pipe: ", client_fd);
		goto error_a;
	}

	wp_log(WP_DBUG, "[Client #%d] Tunnel connection established.", client_fd);

	char *response = "HTTP/1.1 200 OK\r\n\r\n";
	if (send(client_fd, response, strlen(response), 0) <= 0) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to notify client: ", client_fd);
		goto error_b;
	}

	bool connect = true;
	pthread_mutex_lock(&state->lock);
	while (state->is_running && connect) {
		pthread_mutex_unlock(&state->lock);
		if (poll(fds, fds_len, -1) > 0) {
			if (fds[FDS_CLIENT].revents & POLLIN) {
				int len = splice(fds[FDS_CLIENT].fd, NULL, pipes[FDS_CLIENT][1], NULL, MSG_BUFFER_SIZE, SPLICE_F_MOVE);
				splice(pipes[FDS_CLIENT][0], NULL, fds[FDS_SERVER].fd, NULL, len, SPLICE_F_MOVE);
				wp_log(WP_DBUG, "[Client #%d] Sent %d bytes", client_fd, len);
				if (len <= 0) connect = false;
			}
			if (fds[FDS_SERVER].revents & POLLIN) {
				int len = splice(fds[FDS_SERVER].fd, NULL, pipes[FDS_SERVER][1], NULL, MSG_BUFFER_SIZE, SPLICE_F_MOVE);
				splice(pipes[FDS_SERVER][0], NULL, fds[FDS_CLIENT].fd, NULL, len, SPLICE_F_MOVE);
				wp_log(WP_DBUG, "[Client #%d] Recv %d bytes", client_fd, len);
				if (len <= 0) connect = false;
			}
		} else if (errno != EINTR) {
			wp_elog(WP_ERRR, "[Client #%d] An error occurred: ", client_fd);
			connect = false;
		}
		pthread_mutex_lock(&state->lock);
	};
	pthread_mutex_unlock(&state->lock);

error_b:
	wp_log(WP_DBUG, "[Client #%d] Closing tunnel connection...", client_fd);
	close(pipes[FDS_CLIENT][0]); close(pipes[FDS_CLIENT][1]);
	close(pipes[FDS_SERVER][0]); close(pipes[FDS_SERVER][1]);
error_a:
	close(server_fd);
}

void wp_request_handler(void *raw_args) {
	struct wp_handler_args *args = raw_args;
	struct wp_state *state = args->state;
	int client_fd = args->fd;

	wp_log(WP_DBUG, "[Client #%d] Parsing request...", client_fd);

	struct wp_http_msg *request = wp_http_recv(client_fd, RECV_TIMEOUT);
	if (!request) {
		wp_elog(WP_ERRR, "[Client #%d] Failed to receive request: ", client_fd);
		goto error_a;
	}

	bool fail = false;
	char *line, *method, *host, *port, *path;
	CURLU *h = curl_url();
	line = strdup(request->line);
	method = strsep(&line, " ");
	fail |= curl_url_set(h, CURLUPART_URL, strsep(&line, " "), CURLU_DEFAULT_SCHEME);
	fail |= curl_url_get(h, CURLUPART_HOST, &host, 0);
	fail |= curl_url_get(h, CURLUPART_PORT, &port, CURLU_DEFAULT_PORT);
	fail |= curl_url_get(h, CURLUPART_PATH, &path, 0);
	curl_url_cleanup(h);
	if (fail) {
		wp_log(WP_ERRR, "[Client #%d] Failed to parse request line.", client_fd);
		goto error_b;
	}

	wp_log(WP_INFO, "[Client #%d] %s %s:%s %s", client_fd, method, host, port, path);

	pthread_mutex_lock(&state->lock);
	bool blocked = wp_hashset_has(state->blocklist, host);
	pthread_mutex_unlock(&state->lock);
	if (blocked) {
		wp_log(WP_WARN, "[Client #%d] Attempted to connect to blocked domain '%s'. Request denied.", client_fd, host);
		goto error_b;
	}

	strcmp(TUNNEL_METHOD, method) ?
		handle_http_request(state, client_fd, host, port, request) :
		handle_tunnel_request(state, client_fd, host, port);

error_b:
	free(method);
	curl_free(host);
	curl_free(port);
	curl_free(path);
	wp_http_destroy(request);
error_a:
	close(client_fd);
	wp_log(WP_INFO, "[Client #%d] Request concluded.", client_fd);
	free(raw_args);
}
