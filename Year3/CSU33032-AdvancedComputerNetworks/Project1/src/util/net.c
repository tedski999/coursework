#define _GNU_SOURCE
#include "net.h"
#include "log.h"
#include "../config.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>

int wp_open_socket(char *host, char *port, struct addrinfo **addr) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM,
		.ai_flags = host ? 0 : AI_PASSIVE
	};
	if (getaddrinfo(host, port, &hints, addr))
		return -2;
	return socket(addr[0]->ai_family, addr[0]->ai_socktype, addr[0]->ai_protocol);
}

// TODO: this doesn't handle streamed data
// TODO: add error handling (this assumes a perfectly formed http message)
struct wp_http_msg *wp_http_recv(int sockfd, int timeout) {

	// Read all data from sockfd
	struct timeval tv = { timeout, 0 };
	if (setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv))
		return NULL;
	char *data = NULL;
	int data_len = 0, read_len;
	do {
		data = realloc(data, data_len + MSG_BUFFER_SIZE);
		read_len = recv(sockfd, data + data_len, MSG_BUFFER_SIZE - 1, 0);
		data_len += read_len;
	} while (read_len == MSG_BUFFER_SIZE - 1);
	if (read_len < 0) { free(data); return NULL; }
	data[data_len] = '\0';
	setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &(int){0}, sizeof (int));

	// Parse data as HTTP message
	struct wp_http_msg *msg = calloc(1, sizeof *msg);
	// Add first line as request/status line
	msg->line = data;
	char *line_end = strstr(data, "\r\n");
	*line_end = '\0';
	wp_log(WP_DBUG, "HTTP Parse: Line = '%s'", data);
	data = line_end + 2;
	line_end = strstr(line_end + 1, "\r\n");
	// Add next lines as headers until CRLF CRLF encountered
	while (data != line_end) {
		msg->headers = realloc(msg->headers, sizeof *msg->headers * ++msg->headers_len);
		msg->headers[msg->headers_len - 1] = data;
		*line_end = '\0';
		wp_log(WP_DBUG, "HTTP Parse: Header #%d = '%s'", msg->headers_len, data);
		data = line_end + 2;
		line_end = strstr(line_end + 1, "\r\n");
	}
	// Add remaining lines as body
	msg->body = data + 2;
	msg->body_len = strlen(msg->body);
#ifdef PRINT_HTTP_MSGS
	if (msg->body_len)
		wp_log(WP_DBUG, "HTTP Parse: Body (%d bytes):\n%s", msg->body_len, msg->body);
#endif

	return msg;
}

char *wp_http_to_string(struct wp_http_msg *msg) {
	int data_len = strlen(msg->line) + 5; // 5 = CRLF + CRLF + \0
	char *data = malloc(data_len);
	strcat(strcpy(data, msg->line), "\r\n");

	for (int i = 0; i < msg->headers_len; i++) {
		data_len += strlen(msg->headers[i]) + 2; // 2 = CRLF
		data = realloc(data, data_len);
		strcat(strcat(data, msg->headers[i]), "\r\n");
	}

	data_len += strlen(msg->body);
	data = realloc(data, data_len);
	strcat(strcat(data, "\r\n"), msg->body);

#ifdef PRINT_HTTP_MSGS
	wp_log(WP_DBUG, "HTTP String:\n%s", data);
#endif
	return data;
}

void wp_http_destroy(struct wp_http_msg *msg) {
	free(msg->headers);
	free(msg->line);
	free(msg);
}
