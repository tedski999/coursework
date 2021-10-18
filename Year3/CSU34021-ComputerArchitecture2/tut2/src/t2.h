
/*
t2.h - Ted Johnson (TCD 19335618), 2021
Contains C decleration for tut2 functions written in x64 Assembly with NASM syntax (GCC calling convention)
*/

#ifndef TUT2_T2
#define TUT2_T2

#include <inttypes.h>

extern long long inp_int;
extern "C" {
	long long gcd_recursion(long long a, long long b);
	long long use_scanf(long long arr_size, long int *arr); // NOTE: long long *arr is not valid here
	int64_t min(int64_t a, int64_t b, int64_t c);
	int64_t min5(int64_t i, int64_t j, int64_t k, int64_t l);
}

#endif
