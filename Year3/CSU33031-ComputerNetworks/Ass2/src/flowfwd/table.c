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

#include "table.h"
#include <stdlib.h>
#include <string.h>

/*

DUB.c.3
TCD.b.3

DUB:
  a: 4
  b: 4
  c:
    1: 1
    2: 2
    4: 4
TCD:
  a: 4        <- These are different peers to the DUB peers!
  c: 1
  b:
    1: 1
    2: 2
    4: 4

IRE: 1        <- So which peer is this?

*/

/*
struct flowfwd_table {
	char *name;
	int layers; // number of layers until subentries are peers
	struct entry *subentries;
	int subentries_len;
};
*/

struct flowfwd_table {
	char *dst, *peer;
};

static struct flowfwd_table *table;
static int table_len;

// 5   4   3   2 1
// IRE.DUB.tcd.A.maxwell

// A.maxwell
// has 2 length, so should start from layers 1 only

// TODO: this doesn't solve relative addresses
// (every address has to be fully specified, even if they're peers!)
//char *flowfwd_table_get(struct flowfwd_table *table, char **dst, int dst_len) {
char *flowfwd_table_get(char *dst) {
	/*

	if (!table->layer == 1)
		return table->name;

	if (dst_len == table->layers) {

		for (int i = 0; i < table->subentries_len; i++) {
			if (!strcmp(table->subentries[i].name, dst[0])) {
				char *result = flowfwd_table_get(table->subentries + i, dst + 1, dst_len - 1);
				if (result)
					return result;
			}
		}

	} else {

		for (int i = 0; i < table->subentries_len; i++) {
			char *result = flowfwd_table_get(table->subentries + i, dst, dst_len);
			if (result)
				return result;
		}

	}
	*/

	for (int i = 0; i < table_len; i++)
		if (!strcmp(table[i].dst, dst))
			return table[i].peer;
	return NULL;
}

void flowfwd_table_set(char *dst, char *peer) {
	table = realloc(table, sizeof *table * ++table_len);
	table[table_len - 1].dst = dst;
	table[table_len - 1].peer = peer;
}
