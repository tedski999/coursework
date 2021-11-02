#include "protocol.h"
#include "subpubs.h"
#include "jobs.h"
#include "config.h"
#include "util/log.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define PARTIAL_BUFFER_HEADER_LEN 3
#define MIN(a,b) (((a)<(b))?(a):(b))

struct peer_ids {
	struct subpub_net_address address;
	char next_ids[subpub_protocol_request_len];
};

static struct peer_ids *peer_ids_list;
static int peer_ids_list_count;

struct partial_buffer {
	int count, total;
	struct subpub_net_address address;
	char *buffer;
	int buffer_len;
};

static struct partial_buffer *partial_buffers;
static int partial_buffers_count;

static char *get_next_ids(struct subpub_net_address address) {
	for (int i = 0; i < peer_ids_list_count; i++)
		if (!subpub_net_address_cmp(peer_ids_list[i].address, address))
			return peer_ids_list[i].next_ids;
	peer_ids_list = realloc(peer_ids_list, sizeof *peer_ids_list * ++peer_ids_list_count);
	peer_ids_list[peer_ids_list_count - 1] = (struct peer_ids) { address };
	return peer_ids_list[peer_ids_list_count - 1].next_ids;
}

static bool is_valid_id(struct subpub_net_address address, enum subpub_protocol_request request, char id) {
	char *next_ids = get_next_ids(address);
	char expected_id = next_ids[request] & SUBPUB_ID_MASK;

	// TODO: deal with multiple requests coming in unordered, we could do this by maintaining a list of the lowest IDs still not used
	if (id == expected_id) {
		return true;
	} else if (id > expected_id || id > expected_id + SUBPUB_ID_MASK) {
		subpub_log(SUBPUB_LOG_WARN, "Received ID %d when ID %d was expected. Updated expect IDs to match future requests!", id, expected_id);
		next_ids[request] = id;
		return true;
	} else {
		return false;
	}
}

static struct partial_buffer *get_partial_buffer(struct subpub_net_address address) {
	for (int i = 0; i < partial_buffers_count; i++)
		if (!subpub_net_address_cmp(partial_buffers[i].address, address))
			return partial_buffers + i;
	return NULL;
}

static struct partial_buffer *create_partial_buffer(struct subpub_net_address address, int total) {
	int total_len = total * (SUBPUB_MAX_PACKET_LEN - PARTIAL_BUFFER_HEADER_LEN);
	subpub_log(SUBPUB_LOG_DBUG, "Creating new partial buffer to receive up to %d bytes...", total_len);
	partial_buffers = realloc(partial_buffers, sizeof *partial_buffers * ++partial_buffers_count);
	partial_buffers[partial_buffers_count - 1] = (struct partial_buffer) {
		0, total, address, malloc(total_len), total_len
	};
	return partial_buffers + partial_buffers_count - 1;
}

static void clear_partial_buffer(struct subpub_net_address address) {
	for (int i = 0; i < partial_buffers_count; i++) {
		if (!subpub_net_address_cmp(partial_buffers[i].address, address)) {
			subpub_log(SUBPUB_LOG_DBUG, "Clearing partial buffer...");
			free(partial_buffers[i].buffer);
			partial_buffers[i] = partial_buffers[--partial_buffers_count];
			i--;
		}
	}
}

static void send_as_partials(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	int count = buffer_len / (SUBPUB_MAX_PACKET_LEN - PARTIAL_BUFFER_HEADER_LEN) + 1;

	subpub_log(SUBPUB_LOG_INFO, "Splitting 0x%02x into %d partials...", (unsigned char) buffer[0], count);

	for (int i = 0; i < count; i++) {

		// Send partial buffer
		int partial_buffer_len = MIN(SUBPUB_MAX_PACKET_LEN, buffer_len + PARTIAL_BUFFER_HEADER_LEN);
		char *partial_buffer = malloc(partial_buffer_len);
		partial_buffer[0] = subpub_protocol_pack_new_header(address, SUBPUB_REQUEST_PARTIAL);
		partial_buffer[1] = i;
		partial_buffer[2] = count;
		memcpy(partial_buffer + PARTIAL_BUFFER_HEADER_LEN, buffer, partial_buffer_len - PARTIAL_BUFFER_HEADER_LEN);
		subpub_jobs_add(
			SUBPUB_REQUEST_JOB, SUBPUB_TIMEOUT, SUBPUB_RETRIES,
			sockfd, address, partial_buffer, partial_buffer_len);

		// Move on to next partial buffer
		buffer_len -= partial_buffer_len - PARTIAL_BUFFER_HEADER_LEN;
		buffer += partial_buffer_len - PARTIAL_BUFFER_HEADER_LEN;
	}
}

static void unpack_header(char header, enum subpub_protocol_request *request, char *id) {
	*id = header & SUBPUB_ID_MASK;
	*request = (header & SUBPUB_REQUEST_MASK) >> 5;
}

char subpub_protocol_pack_new_header(struct subpub_net_address address, enum subpub_protocol_request request) {
	return (request << 5 & SUBPUB_REQUEST_MASK) | (get_next_ids(address)[request]++ & SUBPUB_ID_MASK);
}

void subpub_protocol_send(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	if (buffer_len <= 0)
		return;
	char header = buffer[0];

	subpub_log(
		SUBPUB_LOG_INFO, "Sending %s (header 0x%02x, %d bytes)...",
		header & SUBPUB_ACK_MASK ? "acknowledgment" : "request", (unsigned char) header, buffer_len);

	if (buffer_len <= SUBPUB_MAX_PACKET_LEN)
		subpub_net_send(sockfd, address, buffer, buffer_len);
	else
		send_as_partials(sockfd, address, buffer, buffer_len);
}

enum subpub_protocol_ack subpub_protocol_handle(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	if (buffer_len <= 0)
		return subpub_protocol_ack_len;
	char header = buffer[0];
	char id; enum subpub_protocol_request request;
	unpack_header(header, &request, &id);

	subpub_log(SUBPUB_LOG_DBUG, "Handling request 0x%02x#%d (header 0x%02x, %d bytes)", request, id, header, buffer_len);

	if (header & SUBPUB_ACK_MASK) {

		// Clear the ack bit, bail if ack is invalid
		header ^= SUBPUB_ACK_MASK;
		if (buffer_len != 2)
			return subpub_protocol_ack_len;

		// Handle acknowledgements
		switch (buffer[1]) {

			case SUBPUB_ACK_OK:
				subpub_log(SUBPUB_LOG_NOTE, "Request 0x%02x#%d acknowledged!", request, id);
				if (!subpub_jobs_remove(header, address))
					subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) header);
				break;

			case SUBPUB_ACK_UNAUTH:
				subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d was not authenticated server-side!", request, id);
				if (!subpub_jobs_remove(header, address))
					subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) header);
				break;

			case SUBPUB_ACK_RESEND:
				subpub_log(SUBPUB_LOG_WARN, "Request 0x%02x#%d failed server-side! Resending...", request, id);
				break;

			case SUBPUB_ACK_STATE:
				subpub_log(
					SUBPUB_LOG_ERRR, "Request 0x%02x#%d failed as your %s subscribed!",
					request, id, (request == SUBPUB_REQUEST_SUBSCRIBE) ? "already" : "not");
				if (!subpub_jobs_remove(header, address))
					subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) header);
				break;

			case SUBPUB_ACK_TOO_LONG:
				subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d was too long for the server to handle!", request, id);
				if (!subpub_jobs_remove(header, address))
					subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) header);
				break;

			case SUBPUB_ACK_TIMEOUT:
				subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d timed-out server-side!", request, id);
				if (!subpub_jobs_remove(header, address))
					subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) header);
				break;

			case SUBPUB_ACK_INVALID:
				subpub_log(SUBPUB_LOG_ERRR, "Request 0x%02x#%d was invalid!", request, id);
				if (!subpub_jobs_remove(header, address))
					subpub_log(SUBPUB_LOG_WARN, "Unable to find and remove command 0x%02x!", (unsigned char) header);
				break;

			default:
				break;
		}

	} else {

		// Watch out for duplicate requests
		if (!is_valid_id(address, request, id)) {
			subpub_log(SUBPUB_LOG_WARN, "Request 0x%02x#%d: Duplicate request (old ID provided)", request, id);
			return SUBPUB_ACK_OK;
		}

		// Mark this request id for this address and request type as used
		get_next_ids(address)[request]++;

		// Handle requests
		switch (request) {

			// Partials are gathered one at a time, then executed one completed.
			// Partials must arrive before the request times-out server-side.
			case SUBPUB_REQUEST_PARTIAL:
				subpub_log(SUBPUB_LOG_NOTE, "Request 0x%02x#%d: Partial", request, id);

				// Parse received partial buffer
				char partial_buffer_index = buffer[1];
				char partial_buffer_total = buffer[2];

				// Get or create the partial buffer for this address and id
				struct partial_buffer *partial_buffer = get_partial_buffer(address);
				if (!partial_buffer) {
					partial_buffer = create_partial_buffer(address, partial_buffer_total);
					subpub_jobs_add(
						SUBPUB_PARTIAL_JOB, SUBPUB_TIMEOUT * SUBPUB_RETRIES, 0,
						sockfd, address, NULL, 0);
				}

				// Verify the expected count equals the requests expected count and that the
				// requests index is less than the total indexes
				if (partial_buffer_total != partial_buffer->total ||
					partial_buffer_index >= partial_buffer_total) {
					subpub_log(SUBPUB_LOG_WARN, "The partial request 0x%02x#%d is invalid!", request, id);
					return SUBPUB_ACK_INVALID;
				}

				// Place the received partial buffer data in the partial buffer
				int partial_buffer_len = buffer_len - PARTIAL_BUFFER_HEADER_LEN;
				subpub_log(
					SUBPUB_LOG_DBUG, "Inserting partial buffer segment %d/%d (%d bytes)...",
					partial_buffer_index + 1, partial_buffer_total, partial_buffer_len);
				int partial_buffer_offset = partial_buffer_index * (SUBPUB_MAX_PACKET_LEN - PARTIAL_BUFFER_HEADER_LEN);
				memcpy(partial_buffer->buffer + partial_buffer_offset, buffer + PARTIAL_BUFFER_HEADER_LEN, partial_buffer_len);
				partial_buffer->count++;
				subpub_log(
					SUBPUB_LOG_DBUG, "Partial buffer has received %d/%d of its expected segments.",
					partial_buffer->count, partial_buffer->total);

				// The last partial buffer dictates the final length of the overall buffer
				if (partial_buffer_index + 1 == partial_buffer_total)
					partial_buffer->buffer_len -= (SUBPUB_MAX_PACKET_LEN - PARTIAL_BUFFER_HEADER_LEN) - partial_buffer_len;

				// If the partial buffer is now complete, execute it
				if (partial_buffer->count >= partial_buffer->total) {
					subpub_log(SUBPUB_LOG_NOTE, "Partial buffer complete, handling request...");
					enum subpub_protocol_ack ack = subpub_protocol_handle(
						sockfd, address, partial_buffer->buffer, partial_buffer->buffer_len);
					if (ack != subpub_protocol_ack_len)
						subpub_protocol_send(sockfd, address, (char[2]) { partial_buffer->buffer[0] | SUBPUB_ACK_MASK, ack }, 2);
					clear_partial_buffer(address);
				}

				return SUBPUB_ACK_OK;

			// Publish requests will be relayed on to all of the request topics subscribers.
			// If this node is a subscriber, the data will be outputted.
			case SUBPUB_REQUEST_PUBLISH: {
				subpub_log(SUBPUB_LOG_NOTE, "Request 0x%02x#%d: Publish", request, id);
				if (buffer[buffer_len - 1])
					return SUBPUB_ACK_INVALID;

				// Parse topic and data from request
				char *topic = buffer + 1;
				char *data = strchr(topic, '\0');
				if (data == buffer + buffer_len - 1)
					return SUBPUB_ACK_INVALID;
				data++;

				// Output data if subscribed
				if (subpub_publishers_is_subscribed(topic, address)) {
					subpub_log(SUBPUB_LOG_INFO, "Subscribed to request 0x%02x#%d, outputting data:", request, id);
					puts(data);
				}

				// Relay request to topic subscribers
				struct subpub_topic_peers subs = subpub_subscribers_get(topic);
				for (int i = 0; i < subs.count; i++) {
					subpub_log(SUBPUB_LOG_INFO, "Relaying request 0x%02x#%d to topic subscriber...", request, id);
					buffer[0] = subpub_protocol_pack_new_header(subs.peers[i], SUBPUB_REQUEST_PUBLISH);
					subpub_jobs_add(
						SUBPUB_REQUEST_JOB, SUBPUB_TIMEOUT, SUBPUB_RETRIES,
						sockfd, subs.peers[i], buffer, buffer_len);
				}

				buffer[0] = header;
				return SUBPUB_ACK_OK;
			}

			// Remove the peer from the subscribers list for the provided topic
			case SUBPUB_REQUEST_UNSUBSCRIBE: {
				subpub_log(SUBPUB_LOG_NOTE, "Request 0x%02x#%d: Unsubscribe", request, id);
				if (buffer[buffer_len - 1])
					return SUBPUB_ACK_INVALID;
				char *topic = buffer + 1;
				return subpub_subscribers_remove(topic, address) ? SUBPUB_ACK_OK : SUBPUB_ACK_STATE;
			}

			// Add the peer top the subscribers list for the provided topic
			case SUBPUB_REQUEST_SUBSCRIBE: {
				subpub_log(SUBPUB_LOG_NOTE, "Request 0x%02x#%d: Subscribe", request, id);
				if (buffer[buffer_len - 1])
					return SUBPUB_ACK_INVALID;
				char *topic = buffer + 1;
				return subpub_subscribers_add(topic, address) ? SUBPUB_ACK_OK : SUBPUB_ACK_STATE;
			}

			default:
				break;
		}
	}

	// By default, don't send an acknowledgment
	return subpub_protocol_ack_len;
}

void subpub_protocol_timeout_partial(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	if (buffer_len <= 0)
		return;
	subpub_protocol_send(sockfd, address, (char[2]) { buffer[0] | SUBPUB_ACK_MASK, SUBPUB_ACK_TIMEOUT }, 2);
	clear_partial_buffer(address);
}

void subpub_protocol_cleanup(void) {
	free(peer_ids_list);
	peer_ids_list = NULL;
	peer_ids_list_count = 0;
}
