#include "address.h"
#include "stddef.h"

struct subpub_net_address subpub_net_address_resolve(char *hostname, char *portname) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_DGRAM,
		.ai_protocol = 0,
		.ai_flags = AI_ADDRCONFIG | (hostname ? 0 : AI_PASSIVE) // TODO: not sure about flags
	};

	struct addrinfo *resolve_list = NULL;
	struct subpub_net_address address = {0};
	if (!getaddrinfo(hostname, portname, &hints, &resolve_list)) {
		// TODO: which resolve_list do we use?
		address.addr = *(struct sockaddr_storage *) resolve_list->ai_addr;
		address.addrlen = resolve_list->ai_addrlen;
		freeaddrinfo(resolve_list);
	}

	return address;
}
