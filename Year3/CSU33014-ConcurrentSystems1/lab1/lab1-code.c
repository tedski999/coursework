//
// CSU33014 Lab 1
//

// Please examine version each of the following routines with names
// starting lab1. Where the routine can be vectorized, please
// complete the corresponding vectorized routine using SSE vector
// intrinsics.

// Note the restrict qualifier in C indicates that "only the pointer
// itself or a value directly derived from it (such as pointer + 1)
// will be used to access the object to which it points".


#include <immintrin.h>
#include <stdio.h>

#include "lab1-code.h"

/****************  routine 0 *******************/

// Here is an example routine that should be vectorized
void lab1_routine0(float * restrict a, float * restrict b, float * restrict c) {
	for (int i = 0; i < 1024; i++) {
		a[i] = b[i] * c[i];
	}
}

// here is a vectorized solution for the example above
void lab1_vectorized0(float * restrict a, float * restrict b, float * restrict c) {
	__m128 a4, b4, c4;
	for (int i = 0; i < 1024; i = i+4) {
		b4 = _mm_loadu_ps(&b[i]);
		c4 = _mm_loadu_ps(&c[i]);
		a4 = _mm_mul_ps(b4, c4);
		_mm_storeu_ps(&a[i], a4);
	}
}

/***************** routine 1 *********************/

// in the following, size can have any positive value
float lab1_routine1(float * restrict a, float * restrict b, int size) {
	float sum = 0.0;
	for (int i = 0; i < size; i++) {
		sum = sum + a[i] * b[i];
	}
	return sum;
}

// insert vectorized code for routine1 here
float lab1_vectorized1(float * restrict a, float * restrict b, int size) {
	int i = 0;
	int remainder = size % 4;

	__m128 sum4 = _mm_setzero_ps();
	for (; i < size - remainder; i += 4) {
		__m128 a4 = _mm_loadu_ps(a + i);
		__m128 b4 = _mm_loadu_ps(b + i);
		__m128 c4 = _mm_mul_ps(a4, b4);
		sum4 = _mm_add_ps(sum4, c4);
	}

	float temp[4];
	_mm_storeu_ps(temp, sum4);
	float sum = temp[0] + temp[1] + temp[2] + temp[3];

	for (; i < size; i++)
		sum += a[i] * b[i];

	return sum;
}

/******************* routine 2 ***********************/

// in the following, size can have any positive value
void lab1_routine2(float * restrict a, float * restrict b, int size) {
	for (int i = 0; i < size; i++) {
		a[i] = 1 - (1.0/(b[i]+1.0));
	}
}

// in the following, size can have any positive value
void lab1_vectorized2(float * restrict a, float * restrict b, int size) {
	int i = 0;
	int remainder = size % 4;

	__m128 one4 = _mm_set1_ps(1);
	for (; i < size - remainder; i += 4) {
		__m128 b4 = _mm_loadu_ps(b + i);
		__m128 divisor4 = _mm_add_ps(b4, one4);
		__m128 quotient4 = _mm_div_ps(b4, divisor4);
		_mm_storeu_ps(a + i, quotient4);
	}

	for (; i < size; i++)
		a[i] = b[i] / (b[i] + 1);
}

/******************** routine 3 ************************/

// in the following, size can have any positive value
void lab1_routine3(float * restrict a, float * restrict b, int size) {
	for (int i = 0; i < size; i++) {
		if (a[i] < 0.0) {
			a[i] = b[i];
		}
	}
}

// in the following, size can have any positive value
void lab1_vectorized3(float * restrict a, float * restrict b, int size) {
	int i = 0;
	int remainder = size % 4;

	__m128 zero4 = _mm_setzero_ps();
	for (; i < size - remainder; i += 4) {
		__m128 a4 = _mm_loadu_ps(a + i);
		__m128 b4 = _mm_loadu_ps(b + i);
		__m128 mask = _mm_cmplt_ps(a4, zero4);
		a4 = _mm_max_ps(a4, zero4);
		b4 = _mm_and_ps(b4, mask);
		a4 = _mm_or_ps(a4, b4);
		_mm_storeu_ps(a + i, a4);
	}

	for (; i < size; i++)
		if (a[i] < 0)
			a[i] = b[i];
}

/********************* routine 4 ***********************/

// hint: one way to vectorize the following code might use
// vector shuffle operations
void lab1_routine4(float * restrict a, float * restrict b, float * restrict c) {
	for (int i = 0; i < 2048; i = i+2) {
		a[i] = b[i]*c[i] - b[i+1]*c[i+1];
		a[i+1] = b[i]*c[i+1] + b[i+1]*c[i];
	}
}

void lab1_vectorized4(float * restrict a, float * restrict b, float * restrict  c) {
	for (int i = 0; i < 2048; i += 4) {
		__m128 b4 = _mm_loadu_ps(b + i);
		__m128 c4 = _mm_loadu_ps(c + i);

		__m128 b4_low     = _mm_shuffle_ps(b4, b4, _MM_SHUFFLE(2,2,0,0)); // = [ b0, b0, b2, b2 ]
		__m128 b4_high    = _mm_shuffle_ps(b4, b4, _MM_SHUFFLE(3,3,1,1)); // = [ b1, b1, b3, b3 ]
		__m128 c4_shuffle = _mm_shuffle_ps(c4, c4, _MM_SHUFFLE(2,3,0,1)); // = [ c1, c0, c3, c2 ]

		__m128 product4_low  = _mm_mul_ps(b4_low,  c4);         // = [ b0*c0, b0*c1, b2*c2, b2*c3 ]
		__m128 product4_high = _mm_mul_ps(b4_high, c4_shuffle); // = [ b1*c1, b1*c0, b3*c3, b3*c2 ]
		__m128 a4 = _mm_addsub_ps(product4_low, product4_high); // = [ b0*c0-b1*c1, b0*c1+b1*c0, b2*c2-b3*c3, b2*c3+b3*c2 ]

		_mm_storeu_ps(a + i, a4);
	}
}

/********************* routine 5 ***********************/

// in the following, size can have any positive value
void lab1_routine5(unsigned char * restrict a, unsigned char * restrict b, int size) {
	for (int i = 0; i < size; i++) {
		a[i] = b[i];
	}
}

void lab1_vectorized5(unsigned char * restrict a, unsigned char * restrict b, int size) {
    __m128i b4;
    int i;
    int remainder = size % 16;
  for (i = 0; i < size; i = i + 16) {
        b4 = _mm_loadu_si128((__m128i *)&b[i]);
    _mm_storeu_si128((__m128i *)&a[i], b4);
  }
    for (; i < size; i++ ) {
    a[i] = b[i];
  }
}

/********************* routine 6 ***********************/

void lab1_routine6(float * restrict a, float * restrict b, float * restrict c) {
	a[0] = 0.0;
	for (int i = 1; i < 1023; i++) {
		float sum = 0.0;
		for (int j = 0; j < 3; j++) {
			sum = sum +  b[i+j-1] * c[j];
		}
		a[i] = sum;
	}
	a[1023] = 0.0;
}

void lab1_vectorized6(float * restrict a, float * restrict b, float * restrict c) {
	a[0] = 0.0;

	int i = 1;
	int remainder = 1023 % 4;

	__m128 c4_0 = _mm_set1_ps(c[0]);
	__m128 c4_1 = _mm_set1_ps(c[1]);
	__m128 c4_2 = _mm_set1_ps(c[2]);
	for (; i < 1023 - remainder; i += 4) {
		__m128 b4_0 = _mm_loadu_ps(b + i - 1);
		__m128 b4_1 = _mm_loadu_ps(b + i);
		__m128 b4_2 = _mm_loadu_ps(b + i + 1);

		__m128 product4_0 = _mm_mul_ps(b4_0, c4_0);
		__m128 product4_1 = _mm_mul_ps(b4_1, c4_1);
		__m128 product4_2 = _mm_mul_ps(b4_2, c4_2);

		__m128 a4 = _mm_add_ps(_mm_add_ps(product4_0, product4_1), product4_2);
		_mm_storeu_ps(a + i, a4);
	}

	for (; i < 1023; i++) {
		float sum = 0.0;
		for (int j = 0; j < 3; j++)
			sum = sum +  b[i + j - 1] * c[j];
		a[i] = sum;
	}

	a[1023] = 0.0;
}

