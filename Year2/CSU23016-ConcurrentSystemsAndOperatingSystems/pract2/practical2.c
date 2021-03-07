#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "cond.c"

int pnum;  // number updated when producer runs.
int csum;  // sum computed using pnum when consumer runs.

int (*pred)(int); // predicate indicating if pnum is to be consumed

bool is_pnum_ready;    // condition
pthread_cond_t cond;   // condition variable
pthread_mutex_t mutex; // mutex lock

int produceT() {
	scanf("%d", &pnum); // read a number from stdin
	return pnum;
}

void *Produce(void *a) {
	int p = 1;
	while (p) {
		pthread_mutex_lock(&mutex);
		while (is_pnum_ready)
			pthread_cond_wait(&cond, &mutex);
		printf("@P-READY\n");
		p = produceT();
		printf("@PRODUCED %d\n", p);
		is_pnum_ready = true;
		pthread_cond_signal(&cond);
		pthread_mutex_unlock(&mutex);
	}
	printf("@P-EXIT\n");
	pthread_exit(NULL);
}

int consumeT() {
	if (pred(pnum))
		csum += pnum;
	return pnum;
}

void *Consume(void *a) {
	int p = 1;
	while (p) {
		pthread_mutex_lock(&mutex);
		while (!is_pnum_ready)
			pthread_cond_wait(&cond, &mutex);
		printf("@C-READY\n");
		p = consumeT();
		printf("@CONSUMED %d\n", csum);
		is_pnum_ready = false;
		pthread_cond_signal(&cond);
		pthread_mutex_unlock(&mutex);
	}
	printf("@C-EXIT\n");
	pthread_exit(NULL);
}

int main(int argc, const char *argv[]) {
	// the current number predicate
	static pthread_t prod, cons;
	long rc;

	pred = &cond1;
	if (argc > 1) {
		if (!strncmp(argv[1], "2", 10))
			pred = &cond2;
		else if (!strncmp(argv[1], "3", 10))
			pred = &cond3;
	}

	pnum = 999;
	csum = 0;
	srand(time(0));

	is_pnum_ready = false;
	pthread_mutex_init(&mutex, NULL);
	pthread_cond_init(&cond, NULL);

	printf("@P-CREATE\n");
	rc = pthread_create(&prod, NULL, Produce, NULL);
	if (rc) {
		printf("@P-ERROR %ld\n", rc);
		exit(-1);
	}
	printf("@C-CREATE\n");
	rc = pthread_create(&cons, NULL, Consume, NULL);
	if (rc) {
		printf("@C-ERROR %ld\n", rc);
		exit(-1);
	}

	printf("@P-JOIN\n");
	pthread_join(prod, NULL);
	printf("@C-JOIN\n");
	pthread_join(cons, NULL);

	pthread_mutex_destroy(&mutex);
	pthread_cond_destroy(&cond);

	printf("@CSUM=%d.\n", csum);
	return 0;
}
