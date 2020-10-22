#include "stack.h"

// create a new empty stack
struct double_stack *double_stack_new(int max_size) {
	struct double_stack *this = malloc(sizeof *this);
	if (!this)
		return NULL;

	this->top = 0;
	this->max_size = max_size;
	this->items = malloc(sizeof (double) * this->max_size);
	if (!this->items) {
		free(this);
		return NULL;
	}
	
	return this;
}

// push a value onto the stack
void double_stack_push(struct double_stack *this, double value) {
	if (this->top >= this->max_size)
		return;

	this->items[this->top++] = value;
}

// pop a value from the stack
double double_stack_pop(struct double_stack *this) {
	if (this->top <= 0)
		return 0;

	return this->items[--this->top];
}

