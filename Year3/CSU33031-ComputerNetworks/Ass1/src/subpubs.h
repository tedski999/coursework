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
