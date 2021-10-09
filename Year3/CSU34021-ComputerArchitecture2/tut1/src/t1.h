
/*
t1.h - Ted Johnson (TCD 19335618), 2021
Contains C decleration for tut1 functions written in x86 Assembly with NASM syntax
*/

#ifndef TUT1_T1
#define TUT1_T1

#include <inttypes.h>

extern "C" int32_t poly(int32_t x);
extern "C" int32_t factorial(int32_t x);
extern "C" void multiple_k_asm(uint16_t m, uint16_t n, uint16_t k, uint16_t arr[]);

#endif
