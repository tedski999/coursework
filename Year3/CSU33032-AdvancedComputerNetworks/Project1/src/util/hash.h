#ifndef WP_UTIL_HASH_H
#define WP_UTIL_HASH_H

#include <stdbool.h>

struct wp_hashset;
struct wp_hashmap;

int wp_hash(const char *str, int mod);

struct wp_hashset *wp_hashset_create(void);
void wp_hashset_add(struct wp_hashset *hashset, const char *str);
bool wp_hashset_has(struct wp_hashset *hashset, const char *str);
void wp_hashset_remove(struct wp_hashset *hashset, const char *str);
void wp_hashset_destroy(struct wp_hashset *hashset);

struct wp_hashmap *wp_hashmap_create(void);
void wp_hashmap_set(struct wp_hashmap *hashmap, const char *str, void *val);
void *wp_hashmap_get(struct wp_hashmap *hashmap, const char *str);
void wp_hashmap_remove(struct wp_hashmap *hashmap, const char *str);
void wp_hashmap_destroy(struct wp_hashmap *hashmap);

#endif

