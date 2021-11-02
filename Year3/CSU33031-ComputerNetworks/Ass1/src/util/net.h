#ifndef SUBPUB_NET_H
#define SUBPUB_NET_H

#include <netdb.h>

struct subpub_net_address { struct sockaddr_storage addr; socklen_t addrlen; };

struct subpub_net_address subpub_net_resolve(char *hostname, char *portname);
int subpub_net_recv(int sockfd, char *buffer, int buffer_len, struct subpub_net_address *src);
int subpub_net_send(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len);
int subpub_net_open(struct subpub_net_address address);
void subpub_net_close(int sockfd);
int subpub_net_address_cmp(struct subpub_net_address a, struct subpub_net_address b);

#endif
