#ifndef WP_UTIL_STRMAP
#define WP_UTIL_STRMAP

struct wp_strmap {
	char *key;
	void *val;
};

struct wp_strmap *wp_strmap_create(void);
void wp_strmap_set(struct wp_strmap *map, const char *key, void *val);
void wp_strmap_unset(struct wp_strmap *map);
void wp_strmap_destroy(struct wp_strmap *map);

#endif
