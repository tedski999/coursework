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

#include "peers.h"
#include "../common/net.h"
#include "../common/log.h"
#include "../common/config.h"
#include <stdlib.h>
#include <string.h>

#define INITIAL_PEERS_CAPACITY 4

static char **peers;
static int peers_len, peers_cap;

static void remove_peer(int index) {
	free(peers[index]);
	peers[index] = peers[--peers_len];
}

void flowfwd_peers_init(void) {
	flowfwd_peers_cleanup();
	peers_cap = INITIAL_PEERS_CAPACITY;
	peers = malloc(sizeof *peers * peers_cap);
}

void flowfwd_peers_list(void) {
	ff_log(FF_LOG_NOTE, "Registered peers:");
	if (!peers_len)
		ff_log(FF_LOG_NOTE, "  <none>");
	for (int i = 0; i < peers_len; i++)
		ff_log(FF_LOG_NOTE, "  %s", peers[i]);
}

void flowfwd_peers_add(char *hostname) {
	ff_log(FF_LOG_DBUG, "Adding a peer with hostname %s...", hostname);
	if (++peers_len > peers_cap)
		peers = realloc(peers, sizeof *peers * (peers_cap *= 2));
	peers[peers_len - 1] = strdup(hostname);
}

int flowfwd_peers_remove(char *hostname) {
	ff_log(FF_LOG_DBUG, "Removing peers with hostname %s...", hostname);
	int peers_removed = 0;
	for (int i = peers_len - 1; i >= 0; i--) {
		if (!strcmp(peers[i], hostname)) {
			remove_peer(i);
			peers_removed++;
		}
	}
	return peers_removed;
}

void flowfwd_peers_command(int argc, char **argv) {

	// List registered peers
	if (argc < 2 || !strcmp(argv[1], "list")) {
		if (argc > 2)
			ff_log(FF_LOG_ERRR, "Usage: %s list", argv[0]);
		else
			flowfwd_peers_list();
	}

	// Add a new peer
	else if (!strcmp(argv[1], "add")) {
		if (argc != 3) {
			ff_log(FF_LOG_ERRR, "Usage: %s add <hostname>", argv[0]);
		} else {
			flowfwd_peers_add(argv[2]);
			ff_log(FF_LOG_NOTE, "Successfully registered a peer with an hostname of %s!", argv[2]);
		}
	}

	// Remove a peer
	else if (!strcmp(argv[1], "remove")) {
		if (argc != 3)
			ff_log(FF_LOG_ERRR, "Usage: %s remove <hostname>", argv[0]);
		else if (flowfwd_peers_remove(argv[2]))
			ff_log(FF_LOG_NOTE, "Successfully removed peers with an hostname of %s!", argv[2]);
		else
			ff_log(FF_LOG_ERRR, "No registered peers found matching %s!", argv[2]);
	}

	// Catch anything else
	else {
		ff_log(FF_LOG_ERRR, "Unknown %s subcommand '%s'!", argv[0], argv[1]);
	}
}

void flowfwd_peers_cleanup(void) {
	for (int i = 0; i < peers_len; i++)
		remove_peer(i);
	free(peers);
	peers = NULL;
	peers_len = 0;
	peers_cap = 0;
}
