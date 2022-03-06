#include "handlers.h"
#include "../cache.h"
#include "../util/log.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void wp_command_handler(struct wp_state *state, int input_fd) {
	wp_log(WP_DBUG, "Handling command socket activity...");

	size_t input_buffer_len;
	char *input_buffer = NULL;
	if (getline(&input_buffer, &input_buffer_len, stdin) == -1) {
		free(input_buffer);
		return;
	}
	input_buffer[strcspn(input_buffer, "\r\n")] = '\0';

	char *cmd = strtok(input_buffer, " ");
	char *arg = strtok(NULL, " ");

	pthread_mutex_lock(&state->lock);

	if (!strcmp("shutdown", cmd)) {
		if (arg) {
			wp_log(WP_ERRR, "Unexpected argument '%s'!", arg);
		} else {
			state->is_running = false;
		}
	} else if (!strcmp("block", cmd)) {
		if (!arg) {
			wp_log(WP_ERRR, "Usage: block <url>");
		} else {
			if (wp_hashset_has(state->blocklist, arg)) {
				wp_log(WP_ERRR, "'%s' is already on the blocklist.", arg);
			} else {
				wp_hashset_add(state->blocklist, arg);
				wp_log(WP_NOTE, "Added '%s' to the blocklist.", arg);
			}
		}
	} else if (!strcmp("unblock", cmd)) {
		if (!arg) {
			wp_log(WP_ERRR, "Usage: unblock <url>");
		} else {
			if (!wp_hashset_has(state->blocklist, arg)) {
				wp_log(WP_ERRR, "'%s' isn't on the blocklist.", arg);
			} else {
				wp_hashset_remove(state->blocklist, arg);
				wp_log(WP_NOTE, "Removed '%s' from the blocklist!", arg);
			}
		}
	} else if (!strcmp("cache", cmd)) {
		wp_cache_print(state->cache, arg);
	} else {
		wp_log(WP_ERRR, "'%s' is not a valid command!", cmd);
	}

	pthread_mutex_unlock(&state->lock);
	free(input_buffer);
}
