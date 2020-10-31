#include "infix.h"

/* Explaination of the algorithm used to evaluate infix expressions:
 *
 * Note that these functions assume that expr is valid and well formed, and makes not attempt at error handling!
 *
 * This algorithm works by splitting the expression into n operators and n+1 operands. A subexpression (Expressions
 * within brackets) is recursively evaluated into an operand. Then the generated operator and operand arrays are
 * looped over until there are no operations left to perform, following operator precedence rules.
 *
 * The postfix notation implementation takes about twice as long in personal experimentation, probably due to
 * memory allocations and better error handling.
 */

#define OPERATORS_COUNT 5
const char OPERATORS[OPERATORS_COUNT] = { '+', '-', 'X', '/', '^' };

static double evaluate_infix_subexpression(char ***expr);

// Evaluate expression stored as an array of string tokens
double evaluate_infix_expression(char **expr, int nterms) {
	int operators_count = 0; // there is always n operators and n+1 operands, so we need only one counter
	char operators[nterms];
	double operands[nterms];

	// generate operand and operator arrays and recursively evaluate bracketed expressions into operands
	char **cur_term = expr, **end_term = expr + nterms;
	while (cur_term < end_term) {
		char *term = *cur_term++;
		double value;
		if (sscanf(term, "%lf", &value) == 1) {
			operands[operators_count] = value;
		} else {
			if (term[0] == '(')
				operands[operators_count] = evaluate_infix_subexpression(&cur_term);
			else
				operators[operators_count++] = term[0]; // increment array counter only after adding the next operator
		}
	}

	// perform the highest precedence operation while they are still operations to perform
	while (operators_count--) {
		int highest_operator_precedence = 0;
		int highest_operator_index = 0;

		// find the highest precedence operation remaining
		for (int cur_operator_index = 0; cur_operator_index <= operators_count; cur_operator_index++) {
			char cur_operator = operators[cur_operator_index];
			int cur_operator_precedence = 0;
			while (cur_operator_precedence < OPERATORS_COUNT && cur_operator != OPERATORS[cur_operator_precedence])
				cur_operator_precedence++;
			if (cur_operator_precedence > highest_operator_precedence) {
				highest_operator_precedence = cur_operator_precedence;
				highest_operator_index = cur_operator_index;
			}
		}

		// perform the operatation
		double rhs = operands[highest_operator_index];
		double lhs = operands[highest_operator_index + 1];
		//printf("%lg %c %lg\n", rhs, OPERATORS[highest_operator_precedence], lhs); // print each operation performed
		switch (OPERATORS[highest_operator_precedence]) {
			case '+': operands[highest_operator_index] = rhs + lhs; break;
			case '-': operands[highest_operator_index] = rhs - lhs; break;
			case 'X': operands[highest_operator_index] = rhs * lhs; break;
			case '/': operands[highest_operator_index] = rhs / lhs; break;
			case '^': operands[highest_operator_index] = pow(rhs, lhs); break;
			default: break;
		}

		// remove used operator and leftover operand from arrays
		for (int shift_index = highest_operator_index + 1; shift_index <= operators_count; shift_index++) {
			operands[shift_index] = operands[shift_index + 1];
			operators[shift_index - 1] = operators[shift_index];
		}
	}

	return operands[0];
}

// Evaluates provided infix expression up to corresponding closing bracket
// The value of expr is set to the element after the corresponding bracket
static double evaluate_infix_subexpression(char ***expr) {
	int bracket_count = 1;
	char **cur_term = *expr;
	do {
		if (**cur_term == '(')
			bracket_count++;
		else if (**cur_term == ')')
			bracket_count--;
		cur_term++;
	} while (bracket_count);

	// the nterms between the corresponding brackets is one less than the difference between these pointers
	double result = evaluate_infix_expression(*expr, cur_term - *expr - 1);
	*expr = cur_term;
	return result;
}

