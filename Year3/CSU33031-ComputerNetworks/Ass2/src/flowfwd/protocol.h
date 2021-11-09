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

#ifndef FLOWFWD_PROTOCOL_H
#define FLOWFWD_PROTOCOL_H

#include "../common/config.h"

void flowfwd_protocol_set_local_address(char *local_address);
enum ff_ack flowfwd_protocol_handle_send_packet(int fd, char **tlvs);
enum ff_ack flowfwd_protocol_handle_client_request(int fd, char **tlvs);
void flowfwd_protocol_address_command(int argc, char **argv);
void flowfwd_protocol_cleanup(void);

#endif
