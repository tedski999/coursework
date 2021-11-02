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

#ifndef SUBPUB_PROTOCOL_H
#define SUBPUB_PROTOCOL_H

#include "util/net.h"

enum subpub_protocol_request {
	SUBPUB_REQUEST_PARTIAL = 0,
	SUBPUB_REQUEST_PUBLISH,
	SUBPUB_REQUEST_UNSUBSCRIBE,
	SUBPUB_REQUEST_SUBSCRIBE,
	subpub_protocol_request_len
};

enum subpub_protocol_ack {
	SUBPUB_ACK_OK = 0,
	SUBPUB_ACK_UNAUTH,
	SUBPUB_ACK_RESEND,
	SUBPUB_ACK_STATE,
	SUBPUB_ACK_TOO_LONG,
	SUBPUB_ACK_TIMEOUT,
	SUBPUB_ACK_INVALID,
	subpub_protocol_ack_len
};

char subpub_protocol_pack_new_header(struct subpub_net_address address, enum subpub_protocol_request request);
void subpub_protocol_send(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len);
enum subpub_protocol_ack subpub_protocol_handle(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len);
void subpub_protocol_timeout_partial(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len);
void subpub_protocol_cleanup(void);

#endif
