#include "request.h"
#include "../net/send.h"
#include "../config.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define IS_SPECIAL_CHAR(c) ((c)==0x00||(c)==0x01||(c)==0x02)
#define PARTIAL_CONTROL_LEN 3
#define MAX_PARTIAL_INDEX 0xff
#define MAX_PARTIAL_DATA_LEN (SUBPUB_MAX_PACKET_LEN - PARTIAL_CONTROL_LEN - 1)

static char next_ids[subpub_request_len] = { 0, 0, 0, 0 };

// TODO: each partial should be ack and resent if not
int send_as_partials(int sockfd, struct subpub_net_address address, char *buffer, int buffer_len) {
	int partials_required = buffer_len / MAX_PARTIAL_DATA_LEN;
	if (partials_required > MAX_PARTIAL_INDEX)
		return -2;

	char header = subpub_protocol_request_pack_new_header(SUBPUB_PARTIAL_REQUEST);
	for (unsigned char index = 0; index <= partials_required; index++) {

		// Pack request into buffer
		int partial_data_len = MIN(MAX_PARTIAL_DATA_LEN, buffer_len);
		int partial_buffer_len = partial_data_len + PARTIAL_CONTROL_LEN;
		char *partial_buffer = malloc(sizeof *buffer * partial_buffer_len);
		partial_buffer[0] = header;
		partial_buffer[1] = index;
		partial_buffer[partial_buffer_len - 1] = SUBPUB_PARTIAL_TERMINATOR_CHARACTER;
		memmove(partial_buffer + 2, buffer, partial_data_len);

		int err = subpub_net_send(sockfd, address, partial_buffer, partial_buffer_len);
		free(partial_buffer);
		if (err < 0)
			return err;

		buffer += partial_data_len;
		buffer_len -= partial_data_len;
	}

	return 0;
}

char subpub_protocol_request_pack_new_header(enum subpub_protocol_request request) {
	return (request << 5 & SUBPUB_REQUEST_MASK) | (next_ids[request]++ & SUBPUB_ID_MASK);
}

int subpub_protocol_request_send(
	int sockfd, struct subpub_net_address address,
	char header, char *topic, char *data) {
	int topic_len = strlen(topic) + 1;
	int data_len = data ? strlen(data) + 1 : 0;

	// TODO: escape characters
	/*
	int special_characters_found = 0;
	for (int i = 0; i < topic_len; i++)
		if (IS_SPECIAL_CHAR(topic[i]))
			special_characters_found++;
	for (int i = 0; i < data_len; i++)
		if (IS_SPECIAL_CHAR(data[i]))
			special_characters_found++;
	*/

	int buffer_len = topic_len + data_len + sizeof header; // + special_characters_found
	char *buffer = malloc(sizeof *buffer * buffer_len);
	buffer[0] = header;

	/*
	int buffer_index = 1;
	for (int i = 0; i <= topic_len; i++) {
		if (IS_SPECIAL_CHAR(topic[i]))
			buffer[buffer_index++] = 0x02;
		buffer[buffer_index++] = topic[i];
	}
	if (data_len) {
		for (int i = 0; i <= data_len; i++) {
			if (IS_SPECIAL_CHAR(data[i]))
				buffer[buffer_index++] = 0x02;
			buffer[buffer_index++] = data[i];
		}
	}
	*/

	memmove(buffer + sizeof header, topic, topic_len);
	if (data)
		memmove(buffer + sizeof header + topic_len, data, data_len);

	// TODO: different error handling
	int err = (buffer_len < SUBPUB_MAX_PACKET_LEN)
		? subpub_net_send(sockfd, address, buffer, buffer_len)
		: send_as_partials(sockfd, address, buffer, buffer_len);
	if (err > 0)
		err = 0;

	free(buffer);
	return err;
}
