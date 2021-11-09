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

#include "clients.h"
#include "../common/net.h"
#include "../common/log.h"
#include "../common/protocol.h"
#include "../common/config.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define INITIAL_CLIENTS_CAPACITY 4

// TODO: eventually should be incorporated into the jobs system
//             ^ callbacks with void *args

struct client { char *src, *portname; };
static struct client *clients;
static int clients_len, clients_cap;

static void remove_client(int index) {
	free(clients[index].src);
	free(clients[index].portname);
	clients[index] = clients[--clients_len];
}

void flowfwd_clients_init(void) {
	flowfwd_clients_cleanup();
	clients_cap = INITIAL_CLIENTS_CAPACITY;
	clients = malloc(sizeof *clients * clients_cap);
}

void flowfwd_clients_list(void) {
	ff_log(FF_LOG_NOTE, "Waiting clients:");
	if (!clients_len)
		ff_log(FF_LOG_NOTE, "  <none>");
	for (int i = 0; i < clients_len; i++)
		ff_log(FF_LOG_NOTE, "  Port %s: %s", clients[i].portname, clients[i].src);
}

void flowfwd_clients_add(char *src, char *portname) {
	ff_log(FF_LOG_DBUG, "Adding a client at port %s waiting for a message from %s...", portname, src);
	if (++clients_len > clients_cap)
		clients = realloc(clients, sizeof *clients * (clients_cap *= 2));
	clients[clients_len - 1].src = strdup(src);
	clients[clients_len - 1].portname = strdup(portname);
}

int flowfwd_clients_fulfill(int fd, char *src, char *msg) {
	ff_log(FF_LOG_DBUG, "Fulfilling clients waiting for a message from %s...", src);
	int clients_fulfilled = 0;
	for (int i = clients_len - 1; i >= 0; i--) {
		if (!strcmp(clients[i].src, src)) {
			ff_log(FF_LOG_DBUG, "  Relaying to a client at port %s...", clients[i].portname);
			struct ff_net_address *address = ff_net_address_create(FF_LOCALHOST, clients[i].portname);
			if (!address) {
				ff_log(FF_LOG_ERRR, "Unable to resolve an address for the client at port %s!", clients[i].portname);
			} else {
				if (!ff_protocol_send_str(fd, address, msg))
					ff_log(FF_LOG_ERRR, "Unable to send data to client at port %s!", clients[i].portname);
				else
					clients_fulfilled++;
				ff_net_address_destroy(address);
			}
			remove_client(i);
		}
	}
	return clients_fulfilled;
}

int flowfwd_clients_remove(char *src, char *portname) {
	ff_log(FF_LOG_DBUG, "Removing clients at port %s waiting for messages from %s...", portname, src);
	int clients_removed = 0;
	for (int i = clients_len - 1; i >= 0; i--) {
		if (!strcmp(clients[i].src, src) && !strcmp(clients[i].portname, portname)) {
			remove_client(i);
			clients_removed++;
		}
	}
	return clients_removed;
}

void flowfwd_clients_command(int argc, char **argv) {

	// List registered peers
	if (argc < 2 || !strcmp(argv[1], "list")) {
		if (argc > 2)
			ff_log(FF_LOG_ERRR, "Usage: %s list", argv[0]);
		else
			flowfwd_clients_list();
	}

	// Add a new waiting client
	else if (!strcmp(argv[1], "add")) {
		if (argc != 4) {
			ff_log(FF_LOG_ERRR, "Usage: %s add <port> <src>", argv[0]);
		} else {
			flowfwd_clients_add(argv[3], argv[2]);
			ff_log(FF_LOG_NOTE, "Successfully registered a client at port %s waiting for a message from %s!", argv[2], argv[3]);
		}
	}

	// Remove a waiting client
	else if (!strcmp(argv[1], "remove")) {
		if (argc != 4)
			ff_log(FF_LOG_ERRR, "Usage: %s remove <port> <src>", argv[0]);
		else if (flowfwd_clients_remove(argv[3], argv[2]))
			ff_log(FF_LOG_NOTE, "Successfully removed clients registered at port %s waiting for a message from %s!", argv[2], argv[3]);
		else
			ff_log(FF_LOG_ERRR, "No clients registered at port %s waiting for a message from %s!", argv[2], argv[3]);
	}

	// Catch anything else
	else {
		ff_log(FF_LOG_ERRR, "Unknown %s subcommand '%s'!", argv[0], argv[1]);
	}
}

void flowfwd_clients_cleanup(void) {
	for (int i = 0; i < clients_len; i++)
		remove_client(i);
	free(clients);
	clients = NULL;
	clients_len = 0;
	clients_cap = 0;
}
