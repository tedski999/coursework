#ifndef WP_HANDLERS_H
#define WP_HANDLERS_H

#include "../state.h"

#define MAX_IPADDR_LEN (40)
#define MAX_PORT_LEN (6)

struct wp_handler_args {
	struct wp_state *state;
	int fd;
};

void wp_command_handler(struct wp_state *state, int input_fd);
void wp_connection_handler(struct wp_state *state, int listening_fd);
void wp_request_handler(void *raw_args);

#endif
