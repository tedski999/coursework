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

#ifndef SUBPUB_HANDLER_H
#define SUBPUB_HANDLER_H

#include "util/net.h"
#include <stdbool.h>

bool subpub_handle_command(int sockfd, struct subpub_net_address *verified_addresses, int verified_addresses_count);
bool subpub_handle_network(int sockfd, struct subpub_net_address *verified_addresses, int verified_addresses_count);

#endif
