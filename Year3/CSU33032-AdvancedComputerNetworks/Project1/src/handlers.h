#ifndef WP_HANDLERS_H
#define WP_HANDLERS_H

#include <stdbool.h>
#include <pthread.h>

struct wp_state {
	pthread_mutex_t lock;
	bool is_running;
	void *blocklist;
	void *cache;
};

void wp_connection_handler(struct wp_state *state, int connection_fd);
void wp_command_handler(struct wp_state *state, int command_fd);

#endif

