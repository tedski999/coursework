#include "list.h"
#include "../protocol/handler.h"
#include "../util/timer.h"
#include "../util/log.h"
#include "../config.h"
#include <stdlib.h>

#define MSECS_IN_SEC 1000
#define MAX(a,b) (((a)>(b))?(a):(b))

enum deadline_type { REQUEST, PARTIAL };

struct subpub_deadline {
	enum deadline_type type;
	struct subpub_timer *timer;
	double attempt_delay;
	int attempts_remaining, sockfd;
	struct subpub_net_address address;
	char header;
	union {
		struct { char *topic, *data; } request;
		struct { int index; } partial;
	} data;
};

struct subpub_deadline_list {
	int count, capacity;
	struct subpub_deadline **items; // TODO: this can just be an array
};

void add_to_list(struct subpub_deadline_list *deadline_list, struct subpub_deadline *deadline) {
	if (++deadline_list->count > deadline_list->capacity) {
		subpub_log(SUBPUB_LOG_DBUG, "Deadline list full, doubling capacity...");
		int new_capacity = deadline_list->capacity * 2;
		deadline_list->items = realloc(deadline_list->items, new_capacity * sizeof deadline_list->items);
		for (int i = deadline_list->capacity; i < new_capacity; i++)
			deadline_list->items[i] = NULL;
		deadline_list->capacity = new_capacity;
	}

	for (int i = 0; i < deadline_list->capacity; i++) {
		if (!deadline_list->items[i]) {
			deadline_list->items[i] = deadline;
			break;
		}
	}
}

struct subpub_deadline *get_next_to_expire(struct subpub_deadline_list *deadline_list) {
	struct subpub_deadline *next_deadline = NULL;
	for (int i = 0; i < deadline_list->capacity; i++)
		if (deadline_list->items[i])
			if (!next_deadline || subpub_timer_measure(deadline_list->items[i]->timer) < subpub_timer_measure(next_deadline->timer))
				next_deadline = deadline_list->items[i];
	return next_deadline;
}

struct subpub_deadline_list *subpub_deadline_list_create(void) {
	struct subpub_deadline_list *deadline_list = malloc(sizeof *deadline_list);
	*deadline_list = (struct subpub_deadline_list) {
		0, SUBPUB_INITIAL_DEADLINE_LIST_CAPACITY,
		calloc(SUBPUB_INITIAL_DEADLINE_LIST_CAPACITY, sizeof *deadline_list->items)
	};
	return deadline_list;
}

void subpub_deadline_list_add_request(
	struct subpub_deadline_list *deadline_list, double attempt_delay, int attempts_remaining,
	int sockfd, struct subpub_net_address address, char header, char *topic, char *data) {
	struct subpub_deadline *deadline = malloc(sizeof *deadline);
	*deadline = (struct subpub_deadline) {
		REQUEST, subpub_timer_create(), attempt_delay, attempts_remaining,
		sockfd, address, header, { .request = { topic, data } }
	};
	add_to_list(deadline_list, deadline);
}

void subpub_deadline_list_add_partial(
	struct subpub_deadline_list *deadline_list, double attempt_delay, int attempts_remaining,
	int sockfd, struct subpub_net_address address, char header, char index) {
	struct subpub_deadline *deadline = malloc(sizeof *deadline);
	*deadline = (struct subpub_deadline) {
		REQUEST, subpub_timer_create(), attempt_delay, attempts_remaining,
		sockfd, address, header, { .partial = { index } }
	};
	add_to_list(deadline_list, deadline);
}

void subpub_deadline_list_set_expired(struct subpub_deadline_list *deadline_list, char header) {
	for (int i = 0; i < deadline_list->capacity; i++)
		if (deadline_list->items[i] && deadline_list->items[i]->header == header)
			subpub_timer_set(deadline_list->items[i]->timer, deadline_list->items[i]->attempt_delay);
}

void subpub_deadline_list_remove(struct subpub_deadline_list *deadline_list, char header) {
	for (int i = 0; i < deadline_list->capacity; i++) {
		if (deadline_list->items[i] && deadline_list->items[i]->header == header) {
			if (deadline_list->items[i]->type == REQUEST) {
				free(deadline_list->items[i]->data.request.topic);
				free(deadline_list->items[i]->data.request.data);
			}
			subpub_timer_destroy(deadline_list->items[i]->timer);
			free(deadline_list->items[i]);
			deadline_list->items[i] = NULL;
		}
	}
}

void subpub_deadline_list_handle_expired(struct subpub_deadline_list *deadline_list) {
	while (1) {
		struct subpub_deadline *deadline = get_next_to_expire(deadline_list);
		if (!deadline || subpub_timer_measure(deadline->timer) < deadline->attempt_delay)
			break;

		if (deadline->attempts_remaining-- <= 0) {
			subpub_log(SUBPUB_LOG_WARN, "Abandoned %s.", (deadline->type == REQUEST) ? "request" : "partial");
			subpub_deadline_list_remove(deadline_list, deadline->header);
			continue;
		}

		switch (deadline->type) {
			case REQUEST:
				subpub_log(SUBPUB_LOG_WARN, "No acknowledgment received for a request. Resending...");
				if (subpub_protocol_request_send(
					deadline->sockfd, deadline->address, deadline->header,
					deadline->data.request.topic, deadline->data.request.data))
					subpub_log(SUBPUB_LOG_WARN, "Unable to resend request!");
				break;
			case PARTIAL:
				// TODO
				subpub_protocol_handle_partial_timeout(
					deadline->sockfd, deadline->address, deadline->header,
					deadline->data.partial.index);
				break;
			default:
				break;
		}

		subpub_timer_set(deadline->timer, 0);
	}
}

int subpub_deadline_list_get_timeout(struct subpub_deadline_list *deadline_list) {
	struct subpub_deadline *next_deadline = get_next_to_expire(deadline_list);
	if (!next_deadline)
		return -1;
	double time_remaining = next_deadline->attempt_delay - subpub_timer_measure(next_deadline->timer);
	return MAX(time_remaining, 0) * 1000;
}

void subpub_deadline_list_destroy(struct subpub_deadline_list *deadline_list) {
	for (int i = 0; i < deadline_list->capacity; i++) {
		if (deadline_list->items[i]) {
			subpub_timer_destroy(deadline_list->items[i]->timer);
			free(deadline_list->items[i]);
		}
	}
	free(deadline_list->items);
	free(deadline_list);
}
