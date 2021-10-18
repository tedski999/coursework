/*
t2Test.cpp - Ted Johnson (TCD 19335618), 2021
Contains C++ implementation for testing tut2 functions written in x64 Assembly with NASM syntax (GCC calling convention)
*/

#include "t2.h"
#include <iostream>
#include <algorithm>
#include <stdlib.h>
#include <time.h>

void check(const char* s, int64_t v, int64_t expected) {
	std::cout << s << " = " << v;
	if (v == expected) {
		std::cout << " OK";
	} else {
		std::cout << " ERROR: should be " << expected;
	}
	std::cout << "\n";
}

int main() {
	/* Checking gcd_recursion */
	std::cout << "GCD Verification\n";
	check("gcd_recursion(35,49)", gcd_recursion(35, 49), 7);
	check("gcd_recursion(48,36)", gcd_recursion(48, 36), 12);
	check("gcd_recursion(104,96)", gcd_recursion(104, 96), 8);
	check("gcd_recursion(1044,2922)", gcd_recursion(1044, 2922), 6);

	// use_scanf
	int64_t sum_scanf;
	int64_t sum_check;
	/* First check of use_scanf */
	const int64_t array_size1 = 5;
	int64_t array1[array_size1] = { 1,2,3,4,5 };
	sum_scanf = use_scanf(array_size1, array1);
	int64_t *min1 = std::max_element(std::begin(array1), std::end(array1));
	sum_check = *min1 + inp_int;
	check("use_scanf(array1)", sum_scanf, sum_check);
	std::cout << "\n";

	/* Second check of use_scanf */
	const int64_t array_size2 = 10;
	int64_t array2[array_size2] = { 3,4,2,3,4,2,0,10,39,-49 };
	sum_scanf = use_scanf(array_size2, array2);
	int64_t* min2 = std::max_element(std::begin(array2), std::end(array2));
	sum_check = *min2 + inp_int;
	check("use_scanf(array2)", sum_scanf, sum_check);
	std::cout << "\n";

	/* Third check of use_scanf */
	const int64_t array_size3 = 100;
	int64_t array3[array_size3];
	srand(time(0));
	for (int i = 0; i < array_size3; ++i)
		array3[i] = (long long)rand() % 1000;
	sum_scanf = use_scanf(array_size3, array3);
	int64_t* min3 = std::max_element(std::begin(array3), std::end(array3));
	sum_check = *min3 + inp_int;
	check("use_scanf(array3)", sum_scanf, sum_check);
	std::cout << "\n\n";

	/* Checking min5 */
	std::cout << "Global variable (inp_int): " << inp_int << "\n";
	check("min5(1, 2, 3, 4)", min5(1, 2, 3, 4), std::min(std::min(std::min(std::min(inp_int, (long long)1), (long long)2), (long long)3), (long long)4));
	check("min5(3, 1, 2, 5)", min5(3, 1, 2, 5), std::min(std::min(std::min(std::min(inp_int, (long long)3), (long long)1), (long long)2), (long long)5));
	check("min5(2, 3, 1, -5)", min5(2, 3, 1, -5), std::min(std::min(std::min(std::min(inp_int, (long long)2), (long long)3), (long long)1), (long long)-5));
	check("min5(-1, -2, -3, -4)", min5(-1, -2, -3, -4), std::min(std::min(std::min(std::min(inp_int, (long long)-1), (long long)-2), (long long)-3), (long long)-4));
	check("min5(-3, -1, -2, 0)", min5(-3, -1, -2, 0), std::min(std::min(std::min(std::min(inp_int, (long long)-3), (long long)-1), (long long)-2), (long long)0));
	check("min5(-2, -3, -1, 3)", min5(-2, -3, -1, 3), std::min(std::min(std::min(std::min(inp_int, (long long)-2), (long long)-3), (long long)-1), (long long)3));
	check("min5(-1, 2, 3, 4)", min5(-1, 2, 3, 4), std::min(std::min(std::min(std::min(inp_int, (long long)-1), (long long)2), (long long)3), (long long)4));
	check("min5(3, -1, 2, 6)", min5(3, -1, 2, 6), std::min(std::min(std::min(std::min(inp_int, (long long)3), (long long)-1), (long long)2), (long long)6));
	check("min5(2, 3, -1, -5)", min5(2, 3, -1, -5), std::min(std::min(std::min(std::min(inp_int, (long long)2), (long long)3), (long long)-1), (long long)-5));

	std::cout << "\n";

	// Code to clear the newline from the buffer and having to wait before exiting
	/*
	int c;
	do {
		c = getchar();
	} while (c != '\n' && c != EOF);
	if (c == EOF) {
		// input stream ended, do something about it, exit perhaps
	} else {
		printf("Type Enter to continue\n");
		getchar();
	}
	*/

	return 0;
}

