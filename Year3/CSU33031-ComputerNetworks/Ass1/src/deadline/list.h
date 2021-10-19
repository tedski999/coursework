#ifndef SUBPUB_DEADLINE_LIST_H
#define SUBPUB_DEADLINE_LIST_H

#include "../protocol/request.h"
#include "../net/address.h"

struct subpub_deadline_list;

struct subpub_deadline_list *subpub_deadline_list_create(void);
void subpub_deadline_list_add_request(
	struct subpub_deadline_list *deadline_list, double attempt_delay, int attempts_remaining,
	int sockfd, struct subpub_net_address address, char header, char *topic, char *data);
void subpub_deadline_list_add_partial(
	struct subpub_deadline_list *deadline_list, double attempt_delay, int attempts_remaining,
	int sockfd, struct subpub_net_address address, char header, char index);
void subpub_deadline_list_set_expired(struct subpub_deadline_list *deadline_list, char header);
void subpub_deadline_list_remove(struct subpub_deadline_list *deadline_list, char header);
void subpub_deadline_list_handle_expired(struct subpub_deadline_list *deadline_list);
int subpub_deadline_list_get_timeout(struct subpub_deadline_list *deadline_list);
void subpub_deadline_list_destroy(struct subpub_deadline_list *deadline_list);

#endif
