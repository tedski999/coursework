/**
 * Subscriber Publisher Protocol
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

#ifndef SUBPUB_SUBPUBS_H
#define SUBPUB_SUBPUBS_H

#include "util/net.h"
#include <stdbool.h>

struct subpub_topic_peers {
	char *topic;
	struct subpub_net_address *peers;
	int count;
};

bool subpub_publishers_add(char *topic, struct subpub_net_address address);
bool subpub_publishers_remove(char *topic, struct subpub_net_address address);
bool subpub_publishers_is_subscribed(char *topic, struct subpub_net_address address);

bool subpub_subscribers_add(char *topic, struct subpub_net_address address);
bool subpub_subscribers_remove(char *topic, struct subpub_net_address address);
struct subpub_topic_peers subpub_subscribers_get(char *topic);

void subpub_subpubs_cleanup(void);

#endif
