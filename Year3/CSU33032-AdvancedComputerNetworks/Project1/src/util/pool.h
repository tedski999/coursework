#ifndef WP_UTIL_POOL_H
#define WP_UTIL_POOL_H

typedef void (*wp_pool_worker)(void *);

struct wp_pool;

struct wp_pool *wp_pool_init(wp_pool_worker worker_func);
void wp_pool_add_job(struct wp_pool *pool, void *job);
void wp_pool_cleanup(struct wp_pool *pool);

#endif

