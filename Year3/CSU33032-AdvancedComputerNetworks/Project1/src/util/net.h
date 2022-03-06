#ifndef WP_UTIL_NET_H
#define WP_UTIL_NET_H

#include "netdb.h"

struct wp_http_msg {
	char *line;
	char **headers, *body;
	int headers_len, body_len;
};

int wp_open_socket(char *host, char *port, struct addrinfo **addr);
struct wp_http_msg *wp_http_recv(int sockfd, int timeout);
char *wp_http_to_string(struct wp_http_msg *msg);
void wp_http_destroy(struct wp_http_msg *msg);

#endif
