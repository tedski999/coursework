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

#include "flowtable.h"
#include "../common/net.h"
#include "../common/config.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

struct entry {
	char *pattern;
	struct ff_net_addr *peer;
};

struct ff_flowfwd_flowtable {
	int count;
	struct entry *entries;
};

struct ff_flowfwd_flowtable *ff_flowfwd_flowtable_create(void) {
	return calloc(1, sizeof (struct ff_flowfwd_flowtable));
}

static bool pattern_match(char *_pattern, char *_target) {
	char *pattern_r, *target_r;
	char delim[2] = { FF_ADDRESS_DELIM };
	char *pattern = strdup(_pattern);
	char *pattern_token = strtok_r(pattern, delim, &pattern_r);
	char *target = strdup(_target);
	char *target_token = strtok_r(target, delim, &target_r);

	// Compare each token between pattern and target
	bool match = false;
	while (pattern_token && target_token) {

		// Match wildcard
		if (!strcmp(pattern_token, FF_ADDRESS_WILDCARD)) {
			match = true;
			break;
		}

		// Reject different tokens
		else if (strcmp(pattern_token, target_token)) {
			match = false;
			break;
		}

		// Move to next tokens
		pattern_token = strtok_r(NULL, delim, &pattern_r);
		target_token = strtok_r(NULL, delim, &target_r);

		// If reached the end of both then match
		if (!pattern_token && !target_token) {
			match = true;
			break;
		}
	}

	free(pattern);
	free(target);
	return match;
}

void ff_flowfwd_flowtable_add(struct ff_flowfwd_flowtable *flowtable, char *pattern, struct ff_net_addr *peer) {
	for (int i = 0; i < flowtable->count; i++) {
		if (!strcmp(flowtable->entries[i].pattern, pattern)) {
			flowtable->entries[i].peer = peer;
			return;
		}
	}

	flowtable->entries = realloc(flowtable->entries, sizeof *flowtable->entries * ++flowtable->count);
	flowtable->entries[flowtable->count - 1] = (struct entry) { strdup(pattern), peer };
}

struct ff_net_addr *ff_flowfwd_flowtable_get(struct ff_flowfwd_flowtable *flowtable, char *destination) {
	for (int i = 0; i < flowtable->count; i++)
		if (pattern_match(flowtable->entries[i].pattern, destination))
			return flowtable->entries[i].peer;
	return NULL;
}

void ff_flowfwd_flowtable_destroy(struct ff_flowfwd_flowtable *flowtable) {
	for (int i = 0; i < flowtable->count; i++)
		free(flowtable->entries[i].pattern);
	free(flowtable->entries);
	free(flowtable);
}

