#include "socket.h"
#include <unistd.h>

int subpub_net_socket_open(struct subpub_net_address address) {
	int sockfd = socket(address.addr.ss_family, SOCK_DGRAM, 0);
	if (sockfd == -1)
		return -1;
	if (bind(sockfd, (struct sockaddr *) &address.addr, address.addrlen) == -1) {
		close(sockfd);
		return -2;
	}
	return sockfd;
}

void subpub_net_socket_close(int sockfd) {
	close(sockfd);
}
