#ifndef SUBPUB_NET_SEND_H
#define SUBPUB_NET_SEND_H

#include "address.h"

int subpub_net_send(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len);

#endif
