/**
 * Flow Forwarding
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

#include "protocol.h"
#include "net.h"
#include "log.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

bool ff_protocol_send_str(int fd, struct ff_net_address *address, char *str) {
	ff_log(FF_LOG_DBUG, "Sending data: %s", str);
	int rc = ff_net_send(fd, address, str, strlen(str));
	return rc;
}

bool ff_protocol_send_tlvs(int fd, struct ff_net_address *address, char **tlvs) {
	ff_log(FF_LOG_DBUG, "Sending TLVs:");
	char *buffer = NULL;
	int buffer_len = 0;
	for (int i = 0; i < ff_data_type_len; i++) {
		if (tlvs[i]) {
			ff_log(FF_LOG_DBUG, "  TLV: 0x%02x - %s", i, tlvs[i]);
			int tlv_len = strlen(tlvs[i]);
			int new_buffer_len = buffer_len + FF_TLV_HEADER_LEN + tlv_len;
			buffer = realloc(buffer, sizeof *buffer * (new_buffer_len + 1)); // + 1 for null terminator if this is the last tlv added
			buffer[buffer_len + 0] = i;
			buffer[buffer_len + 1] = tlv_len;
			memcpy(buffer + buffer_len + FF_TLV_HEADER_LEN, tlvs[i], tlv_len);
			buffer_len = new_buffer_len;
		}
	}
	if (!buffer)
		return false;
	int rc = ff_net_send(fd, address, buffer, buffer_len);
	free(buffer);
	return rc;
}

bool ff_protocol_send_ack(int fd, struct ff_net_address *address, enum ff_ack ack) {
	ff_log(FF_LOG_DBUG, "Sending ack %d as TLVs...", ack);
	char *tlvs[ff_data_type_len] = {0};
	tlvs[FF_DATA_TYPE_ACK] = (char[3]) { FF_DATA_TYPE_ACK, 1, ack };
	return ff_protocol_send_tlvs(fd, address, tlvs);
}

char *ff_protocol_get_next_id(struct ff_net_address *address) {
	char *next_id = strdup("123"); // TODO
	return next_id;
}
