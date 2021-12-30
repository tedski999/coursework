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

#ifndef FF_FLOWCTL_FLOWTABLE_H
#define FF_FLOWCTL_FLOWTABLE_H

#include "../common/net.h"

struct ff_flowctl_service;

struct ff_flowctl_flowtable;

struct ff_flowctl_flowtable *ff_flowctl_flowtable_create(void);
void ff_flowctl_flowtable_generate(
	struct ff_flowctl_flowtable *flowtable, struct ff_flowctl_service *root,
	struct ff_flowctl_service *services, int services_len);
char *ff_flowctl_flowtable_get(struct ff_flowctl_flowtable *flowtable, char *destination);
void ff_flowctl_flowtable_destroy(struct ff_flowctl_flowtable *flowtable);

#endif
