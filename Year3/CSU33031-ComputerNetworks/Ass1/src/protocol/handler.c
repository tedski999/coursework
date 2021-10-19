#include "handler.h"
#include "request.h"
#include "respond.h"
#include "../net/recv.h"
#include "../config.h"

#include "../util/log.h" // TODO: remove me, replace with error returns

void unpack_header(char header, enum subpub_protocol_request *request, char *id) {
	*id = header & SUBPUB_ID_MASK;
	*request = (header & SUBPUB_REQUEST_MASK) >> 5;
}

void handle_subscribe_request(
	int sockfd, struct subpub_net_address address, char header, enum subpub_protocol_request request,
	char id, char *buffer, int buffer_len, struct subpub_deadline_list *deadline_list) {
	char *topic = buffer;
	subpub_log(SUBPUB_LOG_NOTE, "That's a subscribe request. TODO: Add a map between the topic '%s' and the source address to a hash table of subscribers.", topic);
}

void handle_unsubscribe_request(
	int sockfd, struct subpub_net_address address, char header, enum subpub_protocol_request request,
	char id, char *buffer, int buffer_len, struct subpub_deadline_list *deadline_list) {
	char *topic = buffer;
	subpub_log(SUBPUB_LOG_NOTE, "That's an unsubscribe request. TODO: Remove a map between the topic '%s' and the source address in a hash table of subscribers.", topic);
}

void handle_publish_request(
	int sockfd, struct subpub_net_address address, char header, enum subpub_protocol_request request,
	char id, char *buffer, int buffer_len, struct subpub_deadline_list *deadline_list) {
	char *topic = buffer;
	//char *data = "TODO";
	subpub_log(SUBPUB_LOG_NOTE, "That's a publish request. TODO: Relay this publish request to all the addresses mapped to the topic '%s'.", topic);
	//subpub_log(SUBPUB_LOG_NOTE, "The data is: %s", data);
}

void handle_partial_request(
	int sockfd, struct subpub_net_address address, char header, enum subpub_protocol_request request,
	char id, char *buffer, int buffer_len, struct subpub_deadline_list *deadline_list) {
	char index = buffer[0];
	subpub_log(SUBPUB_LOG_NOTE, "That's a partial request. TODO: Collect partial requests together with the same ID and order them by index.");
	subpub_log(SUBPUB_LOG_NOTE, "This partials ID is #%d and its index is %d", id, index);
}

// TODO: error code
void handle_request(
	int sockfd, struct subpub_net_address address, char header, enum subpub_protocol_request request,
	char id, char *buffer, int buffer_len, struct subpub_deadline_list *deadline_list) {
	subpub_log(SUBPUB_LOG_INFO, "Received request 0x%02x#%d...", request, id);

	// TODO: execute request (sub/pub/usb) or add partial to buffer of partials
	switch (request) {
		case SUBPUB_SUBSCRIBE_REQUEST:
			handle_subscribe_request(
				sockfd, address, header, request,
				id, buffer, buffer_len, deadline_list);
			break;
		case SUBPUB_UNSUBSCRIBE_REQUEST:
			handle_unsubscribe_request(
				sockfd, address, header, request,
				id, buffer, buffer_len, deadline_list);
			break;
		case SUBPUB_PUBLISH_REQUEST:
			handle_publish_request(
				sockfd, address, header, request,
				id, buffer, buffer_len, deadline_list);
			break;
		default:
			handle_partial_request(
				sockfd, address, header, request,
				id, buffer, buffer_len, deadline_list);
			break;
	}

	// TODO: temporary OK response to everything
	subpub_log(SUBPUB_LOG_INFO, "Sending 0x%02x (OK) response...", SUBPUB_ACK_OK);
	subpub_protocol_respond_send(sockfd, address, header, SUBPUB_ACK_OK);
}

// TODO: error code
void handle_acknowledgement(
	int sockfd, struct subpub_net_address address, char header, enum subpub_protocol_request request,
	char id, char *buffer, int buffer_len, struct subpub_deadline_list *deadline_list) {

	// TODO: deal with all acks
	header ^= SUBPUB_REQUEST_OR_RESPONSE_MASK;
	switch (buffer[0]) {
		case SUBPUB_ACK_OK:
			subpub_log(SUBPUB_LOG_NOTE, "Request 0x%02x#%d acknowledged!", request, id);
			subpub_deadline_list_remove(deadline_list, header);
			break;
		case SUBPUB_ACK_UNAUTH:
			subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d unauthorized!", request, id);
			subpub_deadline_list_remove(deadline_list, header);
			break;
		case SUBPUB_ACK_TOO_LONG:
			subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d was too long for the server to handle!", request, id);
			subpub_deadline_list_remove(deadline_list, header);
			break;
		case SUBPUB_ACK_RESEND:
			subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d failed server-side!", request, id);
			subpub_deadline_list_set_expired(deadline_list, header);
			break;
		case SUBPUB_ACK_TIMEOUT:
			subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d has timed-out server-side!", request, id);
			subpub_deadline_list_set_expired(deadline_list, header);
			break;
		default:
			subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d returned an unknown acknowledgement code!", request, id);
			break;
	};
}

void subpub_protocol_handle_incoming_data(int sockfd, struct subpub_deadline_list *deadline_list) {
	struct subpub_net_address address;
	char buffer[SUBPUB_MAX_PACKET_LEN];
	int buffer_len = subpub_net_recv(sockfd, buffer, SUBPUB_MAX_PACKET_LEN, &address);
	if (buffer_len == 0 || buffer_len == -1)
		return;

	char header = buffer[0], id;
	enum subpub_protocol_request request;
	unpack_header(header, &request, &id);

	// TODO: authorize / verify source

	if (buffer_len == -2) {
		subpub_protocol_respond_send(sockfd, address, header, SUBPUB_ACK_TOO_LONG);
		return;
	}

	// TODO: unescape characters (0x02 need to be pruned if followed by 0x00, 0x01 or 0x02)

	if (header & SUBPUB_REQUEST_OR_RESPONSE_MASK)
		handle_acknowledgement(
			sockfd, address, header, request, id,
			buffer + sizeof header,
			buffer_len - sizeof header,
			deadline_list);
	else
		handle_request(
			sockfd, address, header, request, id,
			buffer + sizeof header,
			buffer_len - sizeof header,
			deadline_list);
}

void subpub_protocol_handle_partial_timeout(int sockfd, struct subpub_net_address address, char header, char index) {
	// TODO: clear the partial buffer being used
	subpub_log(SUBPUB_LOG_WARN, "partial timeout! 0x%02x#%d", header, index);
	subpub_protocol_respond_send(sockfd, address, header, SUBPUB_ACK_TIMEOUT);
}
