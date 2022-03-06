#include "strset.h"
#include <stdlib.h>
#include <string.h>

struct wp_strset *wp_strset_create(void) {
	return calloc(1, sizeof (struct wp_strset));
}

bool wp_strset_has(struct wp_strset *set, const char *str) {
	for (int i = 0; i < set->len; i++)
		if (!strcmp(set->mem[i], str))
			return true;
	return false;
}

void wp_strset_add(struct wp_strset *set, const char *str) {
	if (!wp_strset_has(set, str)) {
		set->mem = realloc(set->mem, sizeof *set->mem * ++set->len);
		set->mem[set->len - 1] = strdup(str);
	}
}

void wp_strset_remove(struct wp_strset *set, const char *str) {
	for (int i = 0; i < set->len; i++) {
		if (!strcmp(set->mem[i], str)) {
			free(set->mem[i]);
			set->mem[i] = set->mem[--set->len];
		}
	}
}

void wp_strset_destroy(struct wp_strset *set) {
	for (int i = 0; i < set->len; i++)
		free(set->mem[i]);
	free(set->mem);
	free(set);
}
