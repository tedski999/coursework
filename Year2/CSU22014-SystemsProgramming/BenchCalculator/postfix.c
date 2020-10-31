#include "postfix.h"
#include "stack.h"

// evaluate expression stored as an array of string tokens
double evaluate_postfix_expression(char **expr, int nterms) {
	struct double_stack *stack = double_stack_new(nterms);

	for (int i = 0; i < nterms; i++) {
		char *term = expr[i];
		double value;
		if (sscanf(term, "%lf", &value) == 1) {
			double_stack_push(stack, value);
		} else {
			double lhs = double_stack_pop(stack);
			double rhs = double_stack_pop(stack);
			char operator = term[0];
			if (strlen(term) != 1)
				operator = '\0';
			switch (operator) {
				case '+': double_stack_push(stack, rhs + lhs); break;
				case '-': double_stack_push(stack, rhs - lhs); break;
				case 'X': double_stack_push(stack, rhs * lhs); break;
				case '/': double_stack_push(stack, rhs / lhs); break;
				case '^': double_stack_push(stack, pow(rhs, lhs)); break;
				default: printf("Invalid term: '%s'\n", term); exit(1);
			}
		}
	}

	double result = double_stack_pop(stack);
	double_stack_destroy(stack);
	return result;
}

