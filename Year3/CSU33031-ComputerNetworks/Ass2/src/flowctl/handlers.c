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

#include "handlers.h"
#include "flowtable.h"
#include "../common/net.h"
#include "../common/send.h"
#include "../common/requests.h"
#include "../common/config.h"
#include "../common/log.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

bool ff_flowctl_handle_advertisement(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {

	// Is this a service advertisement?
	if (!tlvs[FF_DATATYPE_SRCADDR]) return false;
	if (!tlvs[FF_DATATYPE_PEERADDR]) return false;
	ff_log(FF_LOG_DBUG, "Handling service advertisement...");
	struct ff_flowctl_request_handler_data *data = user_ptr;

	// Find or create the service
	struct ff_flowctl_service *service = NULL;
	for (int i = 0; !service && i < data->services_len; i++)
		if (!strcmp(data->services[i].address, tlvs[FF_DATATYPE_SRCADDR]))
			service = data->services + i;
	if (!service) {
		data->services = realloc(data->services, sizeof *data->services * ++data->services_len);
		service = data->services + data->services_len - 1;
		*service = (struct ff_flowctl_service) {
			strdup(tlvs[FF_DATATYPE_SRCADDR]), 0, NULL, ff_flowctl_flowtable_create()
		};
		ff_log(FF_LOG_INFO, "Registered new service %s", tlvs[FF_DATATYPE_SRCADDR]);
	}

	// Add peer to service if not already added
	bool found = false;
	for (int i = 0; !found && i < service->peers_len; i++)
		if (!strcmp(service->peers[i], tlvs[FF_DATATYPE_PEERADDR]))
			found = true;
	if (!found) {
		service->peers = realloc(service->peers, sizeof *service->peers * ++service->peers_len);
		service->peers[service->peers_len - 1] = strdup(tlvs[FF_DATATYPE_PEERADDR]);
		ff_log(FF_LOG_INFO, "Added %s as peer to service %s", tlvs[FF_DATATYPE_PEERADDR], tlvs[FF_DATATYPE_SRCADDR]);

		// Regenerate flowtables
		for (int i = 0; i < data->services_len; i++) {
			ff_log(FF_LOG_DBUG, "Generating flowtable for %s...", data->services[i].address);
			ff_flowctl_flowtable_generate(
				data->services[i].flowtable, data->services + i,
				data->services, data->services_len);
		}
		ff_log(FF_LOG_DBUG, "Controller flow tables regenerated");
	}

	ff_send_ack(fd, src, FF_ACK_OK);
	return true;
}

bool ff_flowctl_handle_route_request(int fd, struct ff_net_addr *src, struct ff_requests *requests, char *tlvs[ff_datatype_len], void *user_ptr) {

	// Is this a route request?
	if (!tlvs[FF_DATATYPE_SRCADDR]) return false;
	if (!tlvs[FF_DATATYPE_DSTADDR]) return false;
	ff_log(FF_LOG_DBUG, "Handling route request...");
	struct ff_flowctl_request_handler_data *data = user_ptr;

	struct ff_flowctl_service *service = NULL;
	for (int i = 0; !service && i < data->services_len; i++)
		if (!strcmp(data->services[i].address, tlvs[FF_DATATYPE_SRCADDR]))
			service = data->services + i;
	char *pattern = service ? ff_flowctl_flowtable_get(service->flowtable, tlvs[FF_DATATYPE_DSTADDR]) : NULL;

	if (pattern) {
		ff_log(FF_LOG_INFO, "Service %s should forward towards peer %s to get to %s", tlvs[FF_DATATYPE_SRCADDR], pattern, tlvs[FF_DATATYPE_DSTADDR]);
		char *response_tlvs[ff_datatype_len] = {0};
		response_tlvs[FF_DATATYPE_DSTADDR] = tlvs[FF_DATATYPE_DSTADDR];
		response_tlvs[FF_DATATYPE_PEERADDR] = pattern;
		ff_send_tlvs(fd, src, response_tlvs);
	} else {
		ff_log(FF_LOG_INFO, "Service %s cannot get to %s", tlvs[FF_DATATYPE_SRCADDR], tlvs[FF_DATATYPE_DSTADDR]);
		ff_send_ack(fd, src, FF_ACK_NOT_FOUND);
	}

	return true;
}
