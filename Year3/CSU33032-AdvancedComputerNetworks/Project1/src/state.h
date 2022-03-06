#ifndef WP_STATE_H
#define WP_STATE_H

#include "cache.h"
#include "util/pool.h"
#include "util/hash.h"
#include <stdbool.h>
#include <pthread.h>

struct wp_state {
	pthread_mutex_t lock;
	bool is_running;
	struct wp_pool *pool;
	struct wp_hashset *blocklist;
	struct wp_cache *cache;
};

#endif
