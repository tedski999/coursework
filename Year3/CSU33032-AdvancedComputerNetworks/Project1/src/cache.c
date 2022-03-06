#define _GNU_SOURCE
#include "cache.h"
#include "util/hash.h"
#include "util/log.h"
#include "util/net.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

struct cached_response {
	struct wp_http_msg *response;
	time_t expires;
};

struct wp_cache {
	struct wp_hashmap *hashmap;
};

static bool check_cache_control(struct wp_http_msg *msg) {
	for (int i = 0; i < msg->headers_len; i++) {
		char *prefix = "Cache-Control: ";
		int prefix_len = strlen(prefix);
		if (strncasecmp(prefix, msg->headers[i], prefix_len) == 0) {
			char *values = msg->headers[i] + prefix_len;
			if (!strcasestr(values, "public"))
				return false;
		}
	}
	return true;
}

static time_t get_expiring_time(struct wp_http_msg *msg) {
	// TODO: this method of timekeeping isn't sustainable (Y38 bug) although its not exactly world-ending
	// TODO: use these headers instead of assuming 1 hour
	// Cache-Control: max-age=<N>
	// Cache-Control: s-maxage=<N>
	// Expires: <Date>
	// expirationTime = responseTime + freshnessLifetime - currentAge
	return time(NULL) + 60 * 60; // 1 hour from now
}

struct wp_cache *wp_cache_create(void) {
	struct wp_cache *cache = malloc(sizeof *cache);
	cache->hashmap = wp_hashmap_create();
	return cache;
}

void wp_cache_print(struct wp_cache *cache, char *host) {
	wp_log(WP_NOTE, "TODO: Printing the contents of the cache.");
}

bool wp_cache_add(struct wp_cache *cache, struct wp_http_msg *request, struct wp_http_msg *response) {
	bool result = false;

	char *line = strdup(request->line);
	char *method = strsep(&line, " ");
	char *url = strsep(&line, " ");
	if (strcmp(method, "GET"))
		goto error;

	if (!check_cache_control(request) || !check_cache_control(response))
		goto error;

	time_t expires = get_expiring_time(response);
	if (expires <= 0)
		goto error;

	struct cached_response *cached_response = malloc(sizeof *cached_response);
	cached_response->response = response;
	cached_response->expires = expires;
	wp_hashmap_set(cache->hashmap, url, cached_response);

	result = true;
error:
	free(method);
	return result;
}

char *wp_cache_get(struct wp_cache *cache, struct wp_http_msg *request) {
	char *response_data = NULL;

	char *line = strdup(request->line);
	char *method = strsep(&line, " ");
	char *url = strsep(&line, " ");
	if (strcmp(method, "GET"))
		goto error;

	if (!check_cache_control(request))
		goto error;

	struct cached_response *cached_response = wp_hashmap_get(cache->hashmap, url);
	if (!cached_response || time(NULL) >= cached_response->expires)
		goto error;

	response_data = wp_http_to_string(cached_response->response);

error:
	free(method);
	return response_data;
}

void wp_cache_destroy(struct wp_cache *cache) {

	// TODO: not quite cleaned-up
	/*
	for every map in hashmap:
		cached_response = map.val
		free(cached_response->response)
		free(cached_response)
	*/

	wp_hashmap_destroy(cache->hashmap);
	free(cache);
}
