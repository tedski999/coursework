#ifndef WP_UTIL_STRSET
#define WP_UTIL_STRSET

#include <stdbool.h>

struct wp_strset {
	char **mem;
	int len;
};

struct wp_strset *wp_strset_create(void);
bool wp_strset_has(struct wp_strset *set, const char *str);
void wp_strset_add(struct wp_strset *set, const char *str);
void wp_strset_remove(struct wp_strset *set, const char *str);
void wp_strset_destroy(struct wp_strset *set);

#endif
