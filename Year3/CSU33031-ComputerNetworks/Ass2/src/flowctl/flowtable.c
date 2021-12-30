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
#include "handlers.h"
#include "../common/net.h"
#include <stdlib.h>
#include <string.h>

struct entry {
	char *pattern;
	char *peer;
};

struct score {
	char *dst;
	char *peer;
	int score;
};

struct scoreboard {
	int count;
	struct score *scores;
};

struct ff_flowctl_flowtable {
	int count;
	struct entry *entries;
};

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

static void generate_scoreboard(
	struct scoreboard *scoreboard, char *peer,
	struct ff_flowctl_service *root, int score,
	struct ff_flowctl_service *services, int services_len) {

	// Find score for this root on scoreboard
	struct score *score_entry = NULL;
	for (int i = 0; !score_entry && i < scoreboard->count; i++)
		if (!strcmp(scoreboard->scores[i].dst, root->address))
			score_entry = scoreboard->scores + i;

	// Add it if it doesn't exist yet
	if (!score_entry) {
		scoreboard->scores = realloc(scoreboard->scores, sizeof *scoreboard->scores * ++scoreboard->count);
		score_entry = scoreboard->scores + scoreboard->count - 1;
		*score_entry  = (struct score) { root->address, peer, score };
	}

	// If this is a better score, update it and try this roots peers
	if (score <= score_entry->score) {
		score_entry->score = score;
		score_entry->peer = peer;
		for (int i = 0; i < root->peers_len; i++)
			for (int j = 0; j < services_len; j++)
				if (!strcmp(services[j].address, root->peers[i]))
					generate_scoreboard(
						scoreboard, peer,
						services + j, score + 1,
						services, services_len);
	}
}

struct ff_flowctl_flowtable *ff_flowctl_flowtable_create(void) {
	return calloc(1, sizeof (struct ff_flowctl_flowtable));
}

void ff_flowctl_flowtable_generate(
	struct ff_flowctl_flowtable *flowtable, struct ff_flowctl_service *root,
	struct ff_flowctl_service *services, int services_len) {

	// Generate scoreboard
	struct scoreboard scoreboard = { root->peers_len + 1, malloc(sizeof *scoreboard.scores * (root->peers_len + 1)) };
	scoreboard.scores[0] = (struct score) { root->address, root->address, 0 };
	for (int i = 0; i < root->peers_len; i++)
		scoreboard.scores[i + 1] = (struct score) { root->peers[i], root->peers[i], 1 };
	for (int i = 0; i < root->peers_len; i++)
		for (int j = 0; j < services_len; j++)
			if (!strcmp(services[j].address, root->peers[i]))
				generate_scoreboard(
					&scoreboard, services[j].address,
					services + j, 1,
					services, services_len);

	// Use scoreboard to populate pattern flowtable
	// TODO: generate patterns
	//for (int i = 0; i < flowtable->count; i++)
	//	free(flowtable->entries[i].pattern);
	flowtable->count = scoreboard.count;
	flowtable->entries = realloc(flowtable->entries, sizeof *flowtable->entries * flowtable->count);
	for (int i = 0; i < scoreboard.count; i++)
		flowtable->entries[i] = (struct entry) { scoreboard.scores[i].dst, scoreboard.scores[i].peer };

	free(scoreboard.scores);
}

char *ff_flowctl_flowtable_get(struct ff_flowctl_flowtable *flowtable, char *destination) {
	for (int i = 0; i < flowtable->count; i++)
		if (pattern_match(flowtable->entries[i].pattern, destination))
			return flowtable->entries[i].peer;
	return NULL;
}

void ff_flowctl_flowtable_destroy(struct ff_flowctl_flowtable *flowtable) {
	for (int i = 0; i < flowtable->count; i++)
		free(flowtable->entries[i].pattern);
	free(flowtable->entries);
	free(flowtable);
}

