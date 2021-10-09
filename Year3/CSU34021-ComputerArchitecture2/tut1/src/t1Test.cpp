/*
t1test.cpp - Ted Johnson (TCD 19335618), 2021
Contains C++ implementation for testing tut1 functions written in x86 Assembly with NASM syntax
*/

#include "t1.h"
#include <iostream>

void multiple_k(uint16_t M, uint16_t N, uint16_t K, uint16_t* array) {
	for (uint16_t i = M; i < N; ++i) {
		if (i % K == 0)
			array[i] = 1;
		else
			array[i] = 0;
	}
}

void check(const char *s, int v, int expected) {
	std::cout << s << " = " << v;
	if (v == expected)
		std::cout << " OK";
	else
		std::cout << " ERROR: should be " << expected;
	std::cout << "\n";
}

int main() {

	// Evaluating the procedures

	// Polynomial evaluation
	check("poly(2)", poly(2), 15);
	check("poly(3)", poly(3), 40);
	check("poly(-1)", poly(-1), 0);
	check("poly(-2)", poly(-2), -5);

	// Factorial evaluation
	check("factorial(5)", factorial(5), 120);
	check("factorial(4)", factorial(4), 24);
	check("factorial(10)", factorial(7), 5040);
	check("factorial(6)", factorial(6), 720);

	// Multiple_k evluation
	uint16_t K1 = 3;
	const uint16_t N1 = 10;
	const uint16_t M1 = 5;
	uint16_t array_N1[N1] = {0};
	uint16_t array_N2[N1] = {0};
	std::cout << "Array of multiples by C++: \n";
	multiple_k(M1, N1, K1, array_N2);
	for (uint16_t i = 0; i < N1; ++i)
		std::cout << array_N2[i] << " ";
	std::cout << "\n";

	std::cout << "Array of multiples by Assembly: \n";
	multiple_k_asm(M1, N1, K1, array_N1);
	for (uint16_t i = 0; i < N1; ++i)
		std::cout << array_N1[i] << " ";
	std::cout << "\n";

	uint16_t K2 = 7;
	const uint16_t N2 = 50;
	const uint16_t M2 = 20;
	uint16_t array_N3[N2] = {0};
	uint16_t array_N4[N2] = {0};
	std::cout << "Array of multiples by C++: \n";
	multiple_k(M2, N2, K2, array_N4);
	for (uint16_t i = 0; i < N2; ++i)
		std::cout << array_N4[i] << " ";
	std::cout << "\n";

	std::cout << "Array of multiples by Assembly: \n";
	multiple_k_asm(M2, N2, K2, array_N3);
	for (uint16_t i = 0; i < N2; ++i)
		std::cout << array_N3[i] << " ";
	std::cout << "\n";

	//getchar();
	return 0;
}
