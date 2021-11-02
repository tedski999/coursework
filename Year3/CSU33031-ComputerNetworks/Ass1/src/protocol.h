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
