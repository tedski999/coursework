#include "recv.h"
#include <unistd.h>
#include <netdb.h>

int subpub_net_recv(int sockfd, char *buffer, int buffer_len, struct subpub_net_address *src) {
	src->addrlen = sizeof src->addr;
	int data_len = recvfrom(
		sockfd, buffer, buffer_len, 0,
		(struct sockaddr *) &src->addr, &src->addrlen);
	if (data_len < 0)
		return -1;
	else if (data_len >= buffer_len)
		return -2;
	return data_len;
}
