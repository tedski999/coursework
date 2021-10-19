#ifndef SUBPUB_NET_ADDRESS_H
#define SUBPUB_NET_ADDRESS_H

#include <netdb.h>

struct subpub_net_address {
	struct sockaddr_storage addr;
	socklen_t addrlen;
};

struct subpub_net_address subpub_net_address_resolve(char *hostname, char *portname);

#endif
