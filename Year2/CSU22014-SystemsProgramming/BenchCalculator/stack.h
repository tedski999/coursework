#ifndef BC_STACK_H
#define BC_STACK_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct double_stack {
	double *items;
	int max_size;
	int top;
};

struct double_stack *double_stack_new(int max_size);              // create a new empty stack
void double_stack_push(struct double_stack *stack, double value); // push a value onto the stack
double double_stack_pop(struct double_stack *stack);              // pop a value from the stack
void double_stack_destroy(struct double_stack *stack);            // deallocates memory used by stack

#endif
