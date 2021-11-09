/**
 * Flow Forwarding
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

#ifndef FF_PROTOCOL_H
#define FF_PROTOCOL_H

#include "net.h"
#include "config.h"
#include <stdbool.h>

bool ff_protocol_send_str(int fd, struct ff_net_address *address, char *str);
bool ff_protocol_send_tlvs(int fd, struct ff_net_address *address, char **tlvs);
bool ff_protocol_send_ack(int fd, struct ff_net_address *address, enum ff_ack ack);
char *ff_protocol_get_next_id(struct ff_net_address *address);

#endif
