#include "hash.h"
#include "strset.h"
#include "strmap.h"
#include <stdlib.h>
#include <stdbool.h>

#define INITIAL_CAP (128)
#define MAX_STORE_RATIO (0.1)

struct wp_hashset {
	struct wp_strset **sets;
	int len, cap;
};

struct wp_hashmap {
	struct wp_strmap **maps;
	int len, cap;
};

static void expand_and_rehash_set(struct wp_hashset *hashset) {
	int new_cap = hashset->cap * 2;
	struct wp_strset **new_sets = calloc(new_cap, sizeof *hashset->sets);

	// For every member in every set in this hashset
	for (int i = 0; i < hashset->cap; i++) {
		struct wp_strset *set = hashset->sets[i];
		for (int j = 0; set && j < set->len; j++) {
			// Find the new set this member should belong to
			int new_i = wp_hash(set->mem[j], new_cap);
			struct wp_strset *new_set = new_sets[new_i];
			// Allocate set if not yet allocated
			if (!new_set) {
				set = wp_strset_create();
				new_sets[new_i] = set;
			}
			// Add to new set
			new_set->mem = realloc(set->mem, sizeof *set->mem * ++set->len);
			new_set->mem[set->len - 1] = set->mem[j];
		}
		free(set);
	}

	free(hashset->sets);
	hashset->sets = new_sets;
	hashset->cap = new_cap;
}

static void expand_and_rehash_map(struct wp_hashmap *hashmap) {
	int new_cap = hashmap->cap * 2;
	struct wp_strmap **new_maps = calloc(new_cap, sizeof *hashmap->maps);

	// For every map in this hashmap
	for (int i = 0; i < hashmap->cap; i++) {
		struct wp_strmap *map = hashmap->maps[i];
		int new_i = wp_hash(map->key, new_cap);
		struct wp_strmap *new_map = new_maps[new_i];
		// Allocate set if not yet allocated
		if (!new_map) {
			map = wp_strmap_create();
			new_maps[new_i] = map;
		}
		// Set map
		new_map->key = map->key;
		new_map->val = map->val;
		free(map);
	}

	free(hashmap->maps);
	hashmap->maps = new_maps;
	hashmap->cap = new_cap;
}

// djb2 XOR
int wp_hash(const char *str, int mod) {
	int c; unsigned long hash = 5381;
	while ((c = *str++))
		hash = ((hash << 5) + hash) ^ c;
	return hash % mod;
}

struct wp_hashset *wp_hashset_create(void) {
	struct wp_hashset *hashset = malloc(sizeof *hashset);
	hashset->len = 0;
	hashset->cap = INITIAL_CAP;
	hashset->sets = calloc(hashset->cap, sizeof *hashset->sets);
	return hashset;
}

void wp_hashset_add(struct wp_hashset *hashset, const char *str) {
	if (++hashset->len / hashset->cap > MAX_STORE_RATIO)
		expand_and_rehash_set(hashset);
	int i = wp_hash(str, hashset->cap);
	struct wp_strset *set = hashset->sets[i];
	if (!set) {
		set = wp_strset_create();
		hashset->sets[i] = set;
	}
	wp_strset_add(set, str);
}

bool wp_hashset_has(struct wp_hashset *hashset, const char *str) {
	struct wp_strset *set = hashset->sets[wp_hash(str, hashset->cap)];
	return set ? wp_strset_has(set, str) : false;
}

void wp_hashset_remove(struct wp_hashset *hashset, const char *str) {
	struct wp_strset *set = hashset->sets[wp_hash(str, hashset->cap)];
	if (set) wp_strset_remove(set, str);
}

void wp_hashset_destroy(struct wp_hashset *hashset) {
	for (int i = 0; i < hashset->cap; i++)
		if (hashset->sets[i])
			wp_strset_destroy(hashset->sets[i]);
	free(hashset->sets);
	free(hashset);
}

struct wp_hashmap *wp_hashmap_create(void) {
	struct wp_hashmap *hashmap = malloc(sizeof *hashmap);
	hashmap->len = 0;
	hashmap->cap = INITIAL_CAP;
	hashmap->maps = calloc(hashmap->cap, sizeof *hashmap->maps);
	return hashmap;
}

void wp_hashmap_set(struct wp_hashmap *hashmap, const char *str, void *val) {
	if (++hashmap->len / hashmap->cap > MAX_STORE_RATIO)
		expand_and_rehash_map(hashmap);
	int i = wp_hash(str, hashmap->cap);
	struct wp_strmap *map = hashmap->maps[i];
	if (!map) {
		map = wp_strmap_create();
		hashmap->maps[i] = map;
	}
	wp_strmap_set(map, str, val);
}

void *wp_hashmap_get(struct wp_hashmap *hashmap, const char *str) {
	struct wp_strmap *map = hashmap->maps[wp_hash(str, hashmap->cap)];
	return map ? map->val : NULL;
}

void wp_hashmap_remove(struct wp_hashmap *hashmap, const char *str) {
	struct wp_strmap *map = hashmap->maps[wp_hash(str, hashmap->cap)];
	if (map) wp_strmap_unset(map);
}

void wp_hashmap_destroy(struct wp_hashmap *hashmap) {
	for (int i = 0; i < hashmap->cap; i++)
		if (hashmap->maps[i])
			wp_strmap_destroy(hashmap->maps[i]);
	free(hashmap->maps);
	free(hashmap);
}
