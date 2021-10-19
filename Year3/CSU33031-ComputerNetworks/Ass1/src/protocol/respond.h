#ifndef SUBPUB_PROTOCOL_RESPOND_H
#define SUBPUB_PROTOCOL_RESPOND_H

#include "../net/address.h"

enum subpub_ack {
	SUBPUB_ACK_OK = 0,
	SUBPUB_ACK_UNAUTH,
	SUBPUB_ACK_RESEND,
	SUBPUB_ACK_TOO_LONG,
	SUBPUB_ACK_TIMEOUT,
	subpub_ack_len
};

int subpub_protocol_respond_send(
	int sockfd, struct subpub_net_address address,
	char header, enum subpub_ack ack);

#endif
