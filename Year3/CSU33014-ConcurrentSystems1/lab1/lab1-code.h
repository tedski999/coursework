//
// CSU33014 Lab 1 Vector Programming
//
// For instructions see lab1-code.c

#ifndef LAB1_code_H
#define LAB1_code_H

void lab1_routine0(float * restrict a, float * restrict b,
		    float * restrict c);
void lab1_vectorized0(float * restrict a, float * restrict b,
		       float * restrict c);
float lab1_routine1(float * restrict a, float * restrict b,
		     int size);
float lab1_vectorized1(float * restrict a, float * restrict b,
			int size);
void lab1_routine2(float * restrict a, float * restrict b, int size);
void lab1_vectorized2(float * restrict a, float * restrict b, int size);
void lab1_routine3(float * restrict a, float * restrict b, int size);
void lab1_vectorized3(float * restrict a, float * restrict b, int size);
void lab1_routine4(float * restrict a, float * restrict b,
		    float * restrict c);
void lab1_vectorized4(float * restrict a, float * restrict b,
		       float * restrict  c);
void lab1_routine5(unsigned char * restrict a,
		    unsigned char * restrict b, int size);
void lab1_vectorized5(unsigned char * restrict a,
		       unsigned char * restrict b, int size);
void lab1_routine6(float * restrict a, float * restrict b,
		    float * restrict c);
void lab1_vectorized6(float * restrict a, float * restrict b,
		       float * restrict c);

#endif
