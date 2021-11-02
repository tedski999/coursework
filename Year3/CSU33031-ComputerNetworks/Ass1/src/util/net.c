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

#include "net.h"
#include <stddef.h>
#include <unistd.h>
#include <netdb.h>
#include <string.h>

#define MIN(a,b) (((a)<(b))?(a):(b))

struct subpub_net_address subpub_net_resolve(char *hostname, char *portname) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_DGRAM,
		.ai_protocol = 0,
		.ai_flags = AI_ADDRCONFIG | (hostname ? 0 : AI_PASSIVE)
	};

	struct addrinfo *resolve_list = NULL;
	struct subpub_net_address address = {0};
	if (!getaddrinfo(hostname, portname, &hints, &resolve_list)) {
		address.addr = *(struct sockaddr_storage *) resolve_list->ai_addr;
		address.addrlen = resolve_list->ai_addrlen;
		freeaddrinfo(resolve_list);
	} else {
		address.addrlen = 0;
	}

	return address;
}

int subpub_net_recv(int sockfd, char *buffer, int buffer_len, struct subpub_net_address *src) {
	src->addrlen = sizeof src->addr;
	int data_len = recvfrom(
		sockfd, buffer, buffer_len, 0,
		(struct sockaddr *) &src->addr, &src->addrlen);
	if (data_len < 0)
		return -1;
	if (data_len >= buffer_len)
		return -2;
	return data_len;
}

int subpub_net_send(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	return sendto(sockfd, buffer, buffer_len, 0, (struct sockaddr *) &address.addr, address.addrlen);
}

int subpub_net_open(struct subpub_net_address address) {
	int sockfd = socket(address.addr.ss_family, SOCK_DGRAM, 0);
	if (sockfd == -1)
		return -1;
	if (bind(sockfd, (struct sockaddr *) &address.addr, address.addrlen) == -1) {
		close(sockfd);
		return -2;
	}
	return sockfd;
}

void subpub_net_close(int sockfd) {
	close(sockfd);
}

int subpub_net_address_cmp(struct subpub_net_address a, struct subpub_net_address b) {
	if (a.addrlen < b.addrlen) return -1;
	else if (a.addrlen > b.addrlen) return 1;
	return memcmp(&a.addr, &b.addr, a.addrlen);
}
