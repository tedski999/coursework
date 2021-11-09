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

#ifndef FF_HANDLER_H
#define FF_HANDLER_H

#include "config.h"

typedef void (*ff_command_callback)(int argc, char **argv);
typedef enum ff_ack (*ff_packet_callback)(int fd, char *argv[ff_data_type_len]);

struct ff_command_handler {
	char *label;
	ff_command_callback callback;
};

struct ff_packet_handler {
	enum ff_data_type type;
	ff_packet_callback callback;
};

void ff_handler(
	struct ff_command_handler *command_handlers, int command_handlers_len,
	struct ff_packet_handler *packet_handlers, int packet_handlers_len);

#endif
