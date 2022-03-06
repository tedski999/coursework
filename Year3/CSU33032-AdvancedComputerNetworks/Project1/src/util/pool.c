#include "pool.h"
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <signal.h>

struct wp_pool {
	pthread_mutex_t lock;
	pthread_cond_t postjob_cond;
	pthread_cond_t acceptjob_cond;
	wp_pool_worker worker_func;
	pthread_t *workers;
	int workers_len, waiting;
	void *next_job;
	bool is_running;
};

static void *worker_wrapper(void *raw_args) {
	struct wp_pool *pool = raw_args;

	pthread_mutex_lock(&pool->lock);
	while (pool->is_running) {
		// Wait for job to become available
		pool->waiting++;
		while (pool->is_running && !pool->next_job)
			pthread_cond_wait(&pool->postjob_cond, &pool->lock);
		pool->waiting--;
		// Handle is_running = false
		if (!pool->is_running)
			break;
		// Accept and work on job
		void *job_data = pool->next_job;
		pool->next_job = NULL;
		pthread_mutex_unlock(&pool->lock);
		pthread_cond_signal(&pool->acceptjob_cond);
		pool->worker_func(job_data);
		pthread_mutex_lock(&pool->lock);
	}
	pthread_mutex_unlock(&pool->lock);

	return NULL;
}

struct wp_pool *wp_pool_init(wp_pool_worker worker_func) {
	struct wp_pool *pool = calloc(1, sizeof *pool);
	*pool = (struct wp_pool) {
		.lock = PTHREAD_MUTEX_INITIALIZER,
		.acceptjob_cond = PTHREAD_COND_INITIALIZER,
		.postjob_cond = PTHREAD_COND_INITIALIZER,
		.worker_func = worker_func,
		.is_running = true
	};
	return pool;
}

void wp_pool_add_job(struct wp_pool *pool, void *job) {
	pthread_mutex_lock(&pool->lock);
	pool->next_job = job;
	// Add new worker if none available
	if (pool->waiting <= 0) {
		pool->workers = realloc(pool->workers, sizeof *pool->workers * ++pool->workers_len);
		pthread_create(pool->workers + pool->workers_len - 1, NULL, worker_wrapper, pool);
	}
	// Wake up worker and wait for job to be accepted
	pthread_cond_signal(&pool->postjob_cond);
	while (pool->next_job)
		pthread_cond_wait(&pool->acceptjob_cond, &pool->lock);
	pthread_mutex_unlock(&pool->lock);
}

void wp_pool_cleanup(struct wp_pool *pool) {
	// pool closed
	pthread_mutex_lock(&pool->lock);
	pool->is_running = false;
	pthread_mutex_unlock(&pool->lock);
	// Wake up all waiting/blocked workers and wait for them to exit
	pthread_cond_broadcast(&pool->postjob_cond);
	for (int i = 0; i < pool->workers_len; i++) {
		pthread_kill(pool->workers[i], SIGINT);
		pthread_join(pool->workers[i], NULL);
	}
	// Cleanup
	free(pool->workers);
	free(pool);
}
