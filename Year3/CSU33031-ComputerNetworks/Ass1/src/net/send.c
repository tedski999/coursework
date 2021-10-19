#include "send.h"
#include <netdb.h>

// TODO: might need to call multiple times to be safe (but will that fragment our packet?)
int subpub_net_send(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	// TODO: compression?
	return sendto(
		sockfd, buffer, buffer_len, 0,
		(struct sockaddr *) &address.addr, address.addrlen);
}
