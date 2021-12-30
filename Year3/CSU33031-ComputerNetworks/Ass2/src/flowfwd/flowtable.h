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

#ifndef FF_FLOWFWD_FLOWTABLE_H
#define FF_FLOWFWD_FLOWTABLE_H

#include "../common/net.h"

struct ff_flowfwd_flowtable;

struct ff_flowfwd_flowtable *ff_flowfwd_flowtable_create(void);
void ff_flowfwd_flowtable_add(struct ff_flowfwd_flowtable *flowtable, char *pattern, struct ff_net_addr *peer);
struct ff_net_addr *ff_flowfwd_flowtable_get(struct ff_flowfwd_flowtable *flowtable, char *destination);
void ff_flowfwd_flowtable_destroy(struct ff_flowfwd_flowtable *flowtable);

#endif
