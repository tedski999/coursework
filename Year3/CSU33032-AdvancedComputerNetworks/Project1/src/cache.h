#ifndef WP_CACHE_H
#define WP_CACHE_H

#include <stdbool.h>
#include "util/net.h"

struct wp_cache;

struct wp_cache *wp_cache_create(void);
void wp_cache_print(struct wp_cache *cache, char *host);
bool wp_cache_add(struct wp_cache *cache, struct wp_http_msg *request, struct wp_http_msg *response);
char *wp_cache_get(struct wp_cache *cache, struct wp_http_msg *request);
void wp_cache_destroy(struct wp_cache *cache);

#endif
