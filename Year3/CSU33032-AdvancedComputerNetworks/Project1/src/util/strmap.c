#include "strmap.h"
#include <stdlib.h>
#include <string.h>

struct wp_strmap *wp_strmap_create(void) {
	return calloc(1, sizeof (struct wp_strmap));
}

void wp_strmap_set(struct wp_strmap *map, const char *key, void *val) {
	wp_strmap_unset(map);
	map->key = strdup(key);
	map->val = val;
}

void wp_strmap_unset(struct wp_strmap *map) {
	free(map->key);
	map->key = NULL;
	map->val = NULL;
}

void wp_strmap_destroy(struct wp_strmap *map) {
	wp_strmap_unset(map);
	free(map);
}
