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

#ifndef FLOWFWD_CLIENTS_H
#define FLOWFWD_CLIENTS_H

#include <stdbool.h>

void flowfwd_clients_init(void);
void flowfwd_clients_list(void);
void flowfwd_clients_add(char *src, char *portname);
int flowfwd_clients_fulfill(int fd, char *src, char *portname);
int flowfwd_clients_remove(char *src, char *portname);
void flowfwd_clients_command(int argc, char **argv);
void flowfwd_clients_cleanup(void);

#endif
