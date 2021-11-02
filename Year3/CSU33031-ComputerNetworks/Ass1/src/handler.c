#include "handler.h"
#include "protocol.h"
#include "jobs.h"
#include "subpubs.h"
#include "util/net.h"
#include "util/log.h"
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define QUIT_COMMAND_LABEL "quit"
#define QUIT_COMMAND_USAGE ""
#define SUBSCRIBE_COMMAND_LABEL "subscribe"
#define SUBSCRIBE_COMMAND_USAGE "<address> <topic>"
#define UNSUBSCRIBE_COMMAND_LABEL "unsubscribe"
#define UNSUBSCRIBE_COMMAND_USAGE "<address> <topic>"
#define PUBLISH_COMMAND_LABEL "publish"
#define PUBLISH_COMMAND_USAGE "<address> <topic> <data>"

bool is_command(char *command_label, char *user_command) {
	if (command_label == NULL || *command_label == '\0' ||
		user_command == NULL || *user_command == '\0')
		return false;
	while (*user_command)
		if (tolower(*command_label++) != tolower(*user_command++))
			return false;
	return true;
}

bool subpub_handle_command(int sockfd, struct subpub_net_address *verified_addresses, int verified_addresses_count) {

	// Read command from stdin
	size_t input_buffer_len;
	char *input_buffer = NULL;
	if (getline(&input_buffer, &input_buffer_len, stdin) == -1) {
		free(input_buffer);
		return true;
	}
	input_buffer[strcspn(input_buffer, "\r\n")] = '\0';

	// Quit command
	if (is_command(QUIT_COMMAND_LABEL, input_buffer)) {
		free(input_buffer);
		return false;
	}

	// Parse command from input
	char *usage;
	char *command_string = input_buffer;
	char *label = strsep(&command_string, " ");
	char *hostname = strsep(&command_string, ":");
	char *portname = strsep(&command_string, " ");
	char *topic = strsep(&command_string, " ");
	char *data = command_string;
	enum subpub_protocol_request request;
	bool data_required = false;
	if (is_command(SUBSCRIBE_COMMAND_LABEL, label)) {
		request = SUBPUB_REQUEST_SUBSCRIBE;
		label = SUBSCRIBE_COMMAND_LABEL;
		usage = SUBSCRIBE_COMMAND_USAGE;
	} else if (is_command(UNSUBSCRIBE_COMMAND_LABEL, label)) {
		request = SUBPUB_REQUEST_UNSUBSCRIBE;
		label = UNSUBSCRIBE_COMMAND_LABEL;
		usage = UNSUBSCRIBE_COMMAND_USAGE;
	} else if (is_command(PUBLISH_COMMAND_LABEL, label)) {
		request = SUBPUB_REQUEST_PUBLISH;
		label = PUBLISH_COMMAND_LABEL;
		usage = PUBLISH_COMMAND_USAGE;
		data_required = true;
	} else {
		subpub_log(SUBPUB_LOG_ERRR, "Unknown command!");
		free(input_buffer);
		return true;
	}

	// Check if all the required arguments were provided
	if (!hostname || *hostname == '\0' ||
		!portname || *portname == '\0' ||
		!topic || *topic == '\0' ||
		(data_required && (!data || *data == '\0')) ||
		(!data_required && data && *data != '\0')) {
		subpub_log(SUBPUB_LOG_ERRR, "Usage: %s %s", label, usage);
		free(input_buffer);
		return true;
	}

	// Resolve address
	struct subpub_net_address address = subpub_net_resolve(hostname, portname);
	if (!address.addrlen) {
		subpub_log(SUBPUB_LOG_ERRR, "Unable to resolve address!");
		free(input_buffer);
		return true;
	}

	// Verify destination address
	bool verified = false;
	for (int i = 0; i < verified_addresses_count; i++)
		if (!subpub_net_address_cmp(verified_addresses[i], address))
			verified = true;
	if (!verified)
		subpub_log(SUBPUB_LOG_WARN, "The destination address of this command isn't in the verified addresses list so don't expect any replies or acknowledgements to be handled!");

	// Update local list of subscribed topics
	if (request == SUBPUB_REQUEST_SUBSCRIBE)
		subpub_publishers_add(topic, address);
	else if (request == SUBPUB_REQUEST_SUBSCRIBE)
		subpub_publishers_remove(topic, address);

	// Create the command
	int topic_len = strlen(topic) + 1;
	int data_len = (data) ? strlen(data) + 1 : 0;
	int request_buffer_len = 1 + topic_len + data_len;
	char *request_buffer = malloc(request_buffer_len);
	request_buffer[0] = subpub_protocol_pack_new_header(address, request);
	memcpy(request_buffer + 1, topic, topic_len);
	memcpy(request_buffer + 1 + topic_len, data, data_len);
	subpub_log(SUBPUB_LOG_NOTE, "Creating new %s request (command 0x%02x)...", label, request_buffer[0]);
	subpub_jobs_add(
		SUBPUB_REQUEST_JOB, SUBPUB_TIMEOUT, SUBPUB_RETRIES,
		sockfd, address, request_buffer, request_buffer_len);
	free(request_buffer);
	free(input_buffer);
	return true;
}

bool subpub_handle_network(int sockfd, struct subpub_net_address *verified_addresses, int verified_addresses_count) {

	// Read data from network
	struct subpub_net_address address;
	char input_buffer[SUBPUB_MAX_PACKET_LEN + 1];
	int input_buffer_len = subpub_net_recv(sockfd, input_buffer, SUBPUB_MAX_PACKET_LEN + 1, &address);
	if (input_buffer_len == 0 || input_buffer_len == -1)
		return true; // Unable to read request so just bail

	// Verify source address
	bool verified = false;
	for (int i = 0; i < verified_addresses_count; i++)
		if (!subpub_net_address_cmp(verified_addresses[i], address))
			verified = true;
	if (!verified) {
		subpub_log(SUBPUB_LOG_INFO, "Discarding unauthenticated network request...");
		// TODO: unauth can lead to endless back-and-forth
		//subpub_protocol_send(sockfd, address, (char[2]) { input_buffer[0] | SUBPUB_ACK_MASK, SUBPUB_ACK_UNAUTH }, 2);
		return true;
	}

	// Deal with either a new request or an acknowledgment for a previous request
	enum subpub_protocol_ack ack = (input_buffer_len == -2)
		? SUBPUB_ACK_TOO_LONG
		: subpub_protocol_handle(sockfd, address, input_buffer, input_buffer_len);

	// Send back an acknowledgment if there is one
	if (ack != subpub_protocol_ack_len)
		subpub_protocol_send(sockfd, address, (char[2]) { input_buffer[0] | SUBPUB_ACK_MASK, ack }, 2);

	return true;
}
