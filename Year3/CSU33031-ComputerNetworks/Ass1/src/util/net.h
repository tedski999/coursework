/**
 * Subscriber Publisher Protocol
 * Copyright (C) 2021 Ted Johnson TCD 19335618 <edjohnso@tcd.ie>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
