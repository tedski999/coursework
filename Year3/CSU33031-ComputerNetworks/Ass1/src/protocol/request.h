#ifndef SUBPUB_PROTOCOL_REQUEST_H
#define SUBPUB_PROTOCOL_REQUEST_H

#include "../net/address.h"

enum subpub_protocol_request {
	SUBPUB_PARTIAL_REQUEST = 0x0,
	SUBPUB_PUBLISH_REQUEST = 0x1,
	SUBPUB_UNSUBSCRIBE_REQUEST = 0x2,
	SUBPUB_SUBSCRIBE_REQUEST = 0x3,
	subpub_request_len = 4
};

char subpub_protocol_request_pack_new_header(enum subpub_protocol_request request);
int subpub_protocol_request_send(
	int sockfd, struct subpub_net_address address,
	char header, char *topic, char *data);

#endif
