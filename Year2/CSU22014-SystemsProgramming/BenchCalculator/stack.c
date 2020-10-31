#include "stack.h"

// create a new empty stack
struct double_stack *double_stack_new(int max_size) {
	if (max_size < 0) {
		puts("Invalid stack size!");
		exit(1);
	}

	struct double_stack *stack = malloc(sizeof *stack);
	if (!stack) {
		puts("Memory alloc error!");
		exit(1);
	}

	stack->top = 0;
	stack->max_size = max_size;
	stack->items = malloc(sizeof (double) * stack->max_size);
	if (!stack->items) {
		puts("Memory alloc error!");
		exit(1);
	}
	
	return stack;
}

// push a value onto the stack
void double_stack_push(struct double_stack *stack, double value) {
	if (stack->top >= stack->max_size) {
		puts("Stack overflow!");
		exit(1);
	}

	stack->items[stack->top++] = value;
}

// pop a value from the stack
double double_stack_pop(struct double_stack *stack) {
	if (stack->top <= 0) {
		puts("Not enough items to operate on!");
		exit(1);
	}

	return stack->items[--stack->top];
}

// deallocates memory used by stack
void double_stack_destroy(struct double_stack *stack) {
	free(stack->items);
	free(stack);
}

