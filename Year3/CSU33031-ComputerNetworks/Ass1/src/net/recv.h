#ifndef SUBPUB_NET_RECV_H
#define SUBPUB_NET_RECV_H

#include "address.h"

int subpub_net_recv(int sockfd, char *buffer, int buffer_len, struct subpub_net_address *src);

#endif
