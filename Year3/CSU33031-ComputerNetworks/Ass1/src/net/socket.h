#ifndef SUBPUB_NET_SOCKET_H
#define SUBPUB_NET_SOCKET_H

#include "address.h"

int subpub_net_socket_open(struct subpub_net_address address);
void subpub_net_socket_close(int sockfd);

#endif
