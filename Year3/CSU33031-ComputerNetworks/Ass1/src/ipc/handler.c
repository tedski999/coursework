#include "handler.h"
#include "fifo.h"
#include "../net/address.h"
#include "../protocol/request.h"
#include "../util/log.h"
#include "../config.h"
#include <stdlib.h>
#include <ctype.h>
#define _GNU_SOURCE
#include <string.h>

bool is_command(char *command_label, char *user_command) {
	if (command_label == NULL || *command_label == '\0' ||
		user_command == NULL || *user_command == '\0')
		return false;
	while (*user_command)
		if (tolower(*command_label++) != tolower(*user_command++))
			return false;
	return true;
}

bool subpub_ipc_handle(int fd, int sockfd, struct subpub_deadline_list *deadline_list) {
	char *buffer = subpub_ipc_fifo_read(fd);
	if (!buffer)
		return true;

	// Parse command from input
	char *usage;
	char *command_string = buffer;
	char *label = strsep(&command_string, " ");
	char *hostname = strsep(&command_string, " ");
	char *portname = strsep(&command_string, " ");
	char *topic = strsep(&command_string, " ");
	char *data = command_string;
	enum subpub_protocol_request request;
	bool data_required = false;
	if (is_command(SUBPUB_QUIT_COMMAND_LABEL, label)) {
		free(buffer);
		return 0;
	} else if (is_command(SUBPUB_SUBSCRIBE_COMMAND_LABEL, label)) {
		request = SUBPUB_SUBSCRIBE_REQUEST;
		label = SUBPUB_SUBSCRIBE_COMMAND_LABEL;
		usage = SUBPUB_SUBSCRIBE_COMMAND_USAGE;
	} else if (is_command(SUBPUB_UNSUBSCRIBE_COMMAND_LABEL, label)) {
		request = SUBPUB_UNSUBSCRIBE_REQUEST;
		label = SUBPUB_UNSUBSCRIBE_COMMAND_LABEL;
		usage = SUBPUB_UNSUBSCRIBE_COMMAND_USAGE;
	} else if (is_command(SUBPUB_PUBLISH_COMMAND_LABEL, label)) {
		request = SUBPUB_PUBLISH_REQUEST;
		label = SUBPUB_PUBLISH_COMMAND_LABEL;
		usage = SUBPUB_PUBLISH_COMMAND_USAGE;
		data_required = true;
	} else {
		subpub_log(SUBPUB_LOG_ERRR, "Unknown command!");
		free(buffer);
		return true;
	}

	// Check if all the required arguments were provided
	if (!hostname || *hostname == '\0' ||
		!portname || *portname == '\0' ||
		!topic || *topic == '\0' ||
		(data_required && (!data || *data == '\0')) ||
		(!data_required && data && *data != '\0')) {
		subpub_log(SUBPUB_LOG_ERRR, "Usage: %s %s", label, usage);
		free(buffer);
		return true;
	}

	subpub_log(SUBPUB_LOG_NOTE, "Sending a new 0x%02x (%s) request...", request, label);

	struct subpub_net_address address = subpub_net_address_resolve(hostname, portname);
	if (!address.addrlen) {
		subpub_log(SUBPUB_LOG_ERRR, "Unable to resolve address!");
		return true;
		free(buffer);
	}

	char header = subpub_protocol_request_pack_new_header(request);
	char *request_topic = strdup(topic);
	char *request_data = data ? strdup(data) : NULL;
	if (subpub_protocol_request_send(sockfd, address, header, topic, data))
		subpub_log(SUBPUB_LOG_ERRR, "Unable to send request!");
	subpub_deadline_list_add_request(
		deadline_list, SUBPUB_DEADLINE_TIMEOUT, SUBPUB_MAX_REQUEST_ATTEMPTS - 1,
		sockfd, address, header, request_topic, request_data);

	free(buffer);
	return true;
}
