/**
 * Flow Forwarding
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

#ifndef FF_NET_H
#define FF_NET_H

struct ff_net_addr;

int ff_net_recv(int sockfd, char *buffer, int buffer_len, struct ff_net_addr **src);
int ff_net_send(int sockfd, struct ff_net_addr *address, char *buffer, int buffer_len);
int ff_net_open(struct ff_net_addr *address);
void ff_net_close(int sockfd);
struct ff_net_addr *ff_net_addr_create(char *hostname, char *portname);
struct ff_net_addr *ff_net_addr_dup(struct ff_net_addr *address);
//int ff_net_addr_extract(struct ff_net_addr *address, char *hostname, int hostname_len, char *portname, int portname_len);
int ff_net_addr_cmp(struct ff_net_addr *a, struct ff_net_addr *b);
void ff_net_addr_destroy(struct ff_net_addr *address);

#endif
