#ifndef SUBPUB_HANDLER_H
#define SUBPUB_HANDLER_H

#include "util/net.h"
#include <stdbool.h>

bool subpub_handle_command(int sockfd, struct subpub_net_address *verified_addresses, int verified_addresses_count);
bool subpub_handle_network(int sockfd, struct subpub_net_address *verified_addresses, int verified_addresses_count);

#endif
