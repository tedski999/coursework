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

#include "subpubs.h"
#include <stdlib.h>
#include <string.h>

struct topic_list {
	struct subpub_topic_peers *topics;
	int count;
};

static struct topic_list subscriber_topics;
static struct topic_list publisher_topics;

static struct subpub_topic_peers *get_topic_peers(struct topic_list *list, char *topic) {
	for (int i = 0; i < list->count; i++)
		if (!strcmp(list->topics[i].topic, topic))
			return list->topics + i;
	return NULL;
}

static bool add_peer(struct topic_list *list, char *topic, struct subpub_net_address address) {

	// Find the peers assosiated with this topic
	struct subpub_topic_peers *topic_peers = get_topic_peers(list, topic);
	if (!topic_peers) {
		// ...or create it if it doesn't exist yet
		list->topics = realloc(list->topics, sizeof *list->topics * ++list->count);
		topic_peers = list->topics + list->count - 1;
		*topic_peers = (struct subpub_topic_peers) { strdup(topic), NULL, 0 };
	}

	// Check this peer hasn't been added yet
	for (int i = 0; i < topic_peers->count; i++)
		if (!subpub_net_address_cmp(topic_peers->peers[i], address))
			return false;

	// Add this peer to the list of peers assosiated with this topic
	topic_peers->peers = realloc(topic_peers->peers, sizeof *topic_peers->peers * ++topic_peers->count);
	topic_peers->peers[topic_peers->count - 1] = address;
	return true;
}

static bool remove_peer(struct topic_list *list, char *topic, struct subpub_net_address address) {

	// Find the peers assosiated with this topic
	struct subpub_topic_peers *topic_peers = get_topic_peers(list, topic);
	if (!topic_peers)
		return false;

	// Remove this peer from the list of peers assosiated with this topic
	bool found = false;
	for (int i = 0; i < topic_peers->count; i++) {
		if (!subpub_net_address_cmp(topic_peers->peers[i], address)) {
			topic_peers->peers[i] = topic_peers->peers[--topic_peers->count];
			found = true;
			i--;
		}
	}

	return found;
}

bool subpub_publishers_add(char *topic, struct subpub_net_address address) {
	return add_peer(&publisher_topics, topic, address);
}

bool subpub_publishers_remove(char *topic, struct subpub_net_address address) {
	return remove_peer(&publisher_topics, topic, address);
}

bool subpub_publishers_is_subscribed(char *topic, struct subpub_net_address address) {
	struct subpub_topic_peers *topic_peers = get_topic_peers(&publisher_topics, topic);
	if (!topic_peers)
		return false;
	for (int i = 0; i < topic_peers->count; i++)
		if (!subpub_net_address_cmp(topic_peers->peers[i], address))
			return true;
	return false;
}

bool subpub_subscribers_add(char *topic, struct subpub_net_address address) {
	return add_peer(&subscriber_topics, topic, address);
}

bool subpub_subscribers_remove(char *topic, struct subpub_net_address address) {
	return remove_peer(&subscriber_topics, topic, address);
}

struct subpub_topic_peers subpub_subscribers_get(char *topic) {
	struct subpub_topic_peers *topic_peers = get_topic_peers(&subscriber_topics, topic);
	return topic_peers ? *topic_peers : (struct subpub_topic_peers) { topic, NULL, 0 };
}

void subpub_subpubs_cleanup(void) {
	struct topic_list *lists[2] = { &subscriber_topics, &publisher_topics };
	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < lists[i]->count; j++) {
			free(lists[i]->topics[j].topic);
			free(lists[i]->topics[j].peers);
			lists[i]->topics[j].peers = NULL;
			lists[i]->topics[j].count = 0;
		}
		free(lists[i]->topics);
		lists[i]->topics = NULL;
		lists[i]->count = 0;
	}
}
