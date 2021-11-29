/*
 * pascal.c - Ted Johnson (TCD 19335618), 2021
 * Contains implementations in C99 required for questions 2 and 3
 */

#include <stdio.h>
#include <time.h>
#include <limits.h>

#define ROW 30
#define POSITION 20
#define TIMING_REPEATS 50

static int procedure_calls;
static int current_depth, maximum_depth;
static int overflows, underflows;
static int cwp, regwin_count, min_regwins, regwins_pushed;

int compute_pascal(int row, int position) {
	if (position == 1) {
		return 1;
	} else if (position == row) {
		return 1;
	} else {
		return compute_pascal(row - 1, position) + compute_pascal(row - 1, position - 1);
	}
}

int instrumented_compute_pascal(int row, int position) {

	// Record depth and overflows
	procedure_calls++;
	current_depth++;
	if (current_depth > maximum_depth) {
		maximum_depth = current_depth;
	}
	if (current_depth > cwp + regwin_count) {
		overflows++;
		cwp++;
		regwins_pushed++;
	}

	// compute_pascal implementation
	int result;
	if (position == 1) {
		result = 1;
	} else if (position == row) {
		result = 1;
	} else {
		result = instrumented_compute_pascal(row - 1, position) + instrumented_compute_pascal(row - 1, position - 1);
	}

	// Record underflows
	current_depth--;
	if (current_depth < cwp + min_regwins) {
		underflows++;
		cwp--;
	}

	return result;
}

int main(int argc, char **argv) {

	/**
	 * Question 2
	 *
	 * Here we are instrumenting the compute_pascal function in the context of
	 * running on RISC-1 architecture. We record several parameters to do with
	 * register windows depth and saving them to stack.
	 *
	 * We perform this examination on compute_pascal(30, 20) and allow the
	 * architecture to have 6, 8 and 16 register windows. We also change the
	 * minimum number of active register windows before underflowing from 2 to 1.
	 *
	 *           Table of overflow count when
	 *         executing compute_pascal(30, 20)
	 *  (register windows vs min active register windows)
	 *
	 *                   2         1
	 *                  ------------------
	 *              6  | 7051107 | 4527433
	 *              8  | 2840175 | 1744180
	 *              16 | 30824   | 15912
	 *
	 * By increasing the number of register windows available, the number of
	 * overflows that occurred dropped rapidly. This performs as expected, as
	 * we saw this explained in lectures.
	 * Additionally, by decreasing the minimum number of active register windows
	 * from 2 to 1, we see another rapid decline in overflows. For this
	 * particular algorithm, the number of overflows appears to decrease by half.
	 *
	 * See provided screenshot for terminal output.
	 */

	enum { regwin_count_len = 3, min_regwins_len = 2 };
	int regwin_count_arr[regwin_count_len] = { 6, 8, 16 };
	int min_regwins_arr[min_regwins_len] = { 2, 1 };
	for (int i = 0; i < min_regwins_len; i++) {
		for (int j = 0; j < regwin_count_len; j++) {

			regwin_count = regwin_count_arr[j];
			min_regwins = min_regwins_arr[i];
			procedure_calls = 0;
			current_depth = maximum_depth = 0;
			overflows = underflows = 0;
			regwins_pushed = cwp = 0;

			printf("\ncompute_pascal(%d, %d) = %d\n", ROW, POSITION, instrumented_compute_pascal(ROW, POSITION));
			printf(" - %d register windows, min %d active\n", regwin_count, min_regwins);
			printf(" - %d procedure calls\n", procedure_calls);
			printf(" - Maximum register window depth was %d", maximum_depth);
			printf(" - There were %d overflows and %d underflows\n", overflows, underflows);
			printf(" - %d register windows were pushed onto the stack\n", regwins_pushed);
		}
	}

	printf("\n");

	/**
	 * Question 3
	 *
	 * On average, compute_pascal(30, 20) takes 25.07ms to complete,
	 * or 24.72ms of CPU time on my machine. I am running Linux on
	 * a 64-bit Intel i5-8300H CPU @ 2.30GHz
	 *
	 * I decided to record both the wall time duration and CPU time duration
	 * as this allows us to see if the operating system is having a significant
	 * impact on the results.
	 *
	 * The accuracy of these results is questionable. I decided to include both
	 * the first and range of values received when running this multiple times,
	 * as I believe running the same function with the same parameters multiple
	 * times may allow the processor to artificially increase performance.
	 *
	 * See provided screenshot for terminal output.
	 */

	double time = 0, first_time = 0, min_time = INT_MAX, max_time = 0;
	printf("Wall timing compute_pascal(%d, %d) %d times...\n", ROW, POSITION, TIMING_REPEATS);
	for (int i = 0; i < TIMING_REPEATS; i++) {
		struct timespec s, e;
		clock_gettime(CLOCK_MONOTONIC, &s);
		compute_pascal(ROW, POSITION);
		clock_gettime(CLOCK_MONOTONIC, &e);
		double new_time = ((double)(e.tv_sec*1000000 + e.tv_nsec/1000) - (s.tv_sec*1000000 + s.tv_nsec/1000)) / 1000;
		if (!first_time) first_time = new_time;
		if (new_time < min_time) min_time = new_time;
		if (new_time > max_time) max_time = new_time;
		time += new_time;
	}
	time /= TIMING_REPEATS;

	double cpu_time = 0, cpu_first_time = 0, cpu_min_time = INT_MAX, cpu_max_time = 0;
	printf("CPU timing compute_pascal(%d, %d) %d times...\n", ROW, POSITION, TIMING_REPEATS);
	for (int i = 0; i < TIMING_REPEATS; i++) {
		clock_t start = clock();
		compute_pascal(ROW, POSITION);
		double new_time = ((double)(clock() - start) / CLOCKS_PER_SEC) * 1000;
		if (!cpu_first_time) cpu_first_time = new_time;
		if (new_time < cpu_min_time) cpu_min_time = new_time;
		if (new_time > cpu_max_time) cpu_max_time = new_time;
		cpu_time += new_time;
	}
	cpu_time /= TIMING_REPEATS;

	printf("%fms average execution wall time (first %fms, range %fms - %fms)\n", time, first_time, min_time, max_time);
	printf("%fms average execution CPU time (first %fms, range %fms - %fms)\n", cpu_time, cpu_first_time, cpu_min_time, cpu_max_time);

	return 0;
}
