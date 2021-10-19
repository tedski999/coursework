#include "respond.h"
#include "../net/socket.h"
#include "../net/send.h"
#include "../config.h"

int subpub_protocol_respond_send(int sockfd, struct subpub_net_address address, char header, enum subpub_ack ack) {
	header |= SUBPUB_REQUEST_OR_RESPONSE_MASK;
	int buffer_len = sizeof header + 1; // + 1 for ack
	char buffer[sizeof header + 1];
	buffer[0] = header;
	buffer[sizeof header] = ack;
	int err = subpub_net_send(sockfd, address, buffer, buffer_len);
	if (err > 0)
		err = 0;
	return err;
}
