// -------------------------------------------------------------------------
/**
 *  Utility class containing validation/evaluation/conversion operations
 *  for infix and postfix arithmetic expressions.
 *
 *  @author Ted Johnson
 *  @version 16/12/20
 */

import java.util.Stack;

public class Arith {

	private static final String VALID_OPERATORS = "+-*/";

	// NOTE: Java enums and WebCat code coverage don't play nice together
	//       (enums generate some bytecode for the valueOf method, which I don't use).
	//       Outside of the assignment, I would use this enum for the validateInfixOrder states.
	//private static enum ValidationState {
	//	OPERAND_EXPECTED,
	//	OPERATOR_EXPECTED,
	//	CLOSING_PARENTHESIS_EXPECTED
	//}
	private static final int OPERAND_EXPECTED = 0;
	private static final int OPERATOR_EXPECTED = 1;
	private static final int CLOSING_PARENTHESIS_EXPECTED = 2;
	
	/**
	 * Validation method for integer string.
	 * Time complexity: O(n) for n character strings
	 * Justification: Each character in the array is check only once.
	 *
	 * I believe this is the optimal method to test if a string represents an integer,
	 * as every character is only checked once, even if a minus sign is used.
	 * I chose to increment over .charAt() instead of first using .toCharArray() because
	 * it make the code a little easier to read.
	 * Another possible method is to use Integer.parseInt and catch any exceptions, but
	 * it is generally considered bad practice to use exceptions for control flow.
	 *
	 * NOTE: Reflective programming disallowed by WebCat, so in order to
	 *       properly test the method, it has been made public instead.
	 *
	 * @param string : a string to be checked if it represents a integer.
	 *
	 * @return true if the parameter is indeed a valid integer, and false otherwise.
	 **/
	public static boolean isStringInteger(String string) {
		if (string == null || string.length() == 0)
			return false;

		// If the first character isn't a number of '-', or '-' is the only character, return false
		if (!Character.isDigit(string.charAt(0)) && (string.charAt(0) != '-' || string.length() == 1))
			return false;

		// Check if the remaining characters are numeric
		for (int i = 1; i < string.length(); i++)
			if (!Character.isDigit(string.charAt(i)))
				return false;

		return true;
	}


	//~ Validation methods ..........................................................

	/**
	 * Validation method for infix notation.
	 * Time complexity: O(n^2) for n elements in infixLiterals.
	 * Justification: With a length of n, infixLiterals is iterated over just once.
	 *
	 * To validate the expression, I decided to use a rudimentary state machine with a stack.
	 * A valid infix expression is either a single operand or a recursive structure of ( operand operator operand ),
	 * where an operand can be another infix expression - a subexpression.
	 * I believe this method is optimal as the time complexity is the theoretical best:
	 * It is not possible to validate the expression without checking each element in infixLiterals at least once.
	 * An alternative would have been to use RegEx with Pattern and Matcher.
	 * However, modern RegEx has a terrible and unpredictable time complexity of upwards above 2^n for n characters.
	 *
	 * @param infixLiterals : an array containing the string literals hopefully in infix order.
	 * The method assumes that each of these literals can be one of:
	 * - "+", "-", "*", or "/"
	 * - or a valid string representation of an integer "0", "1" , "2", ..., "-1", "-2", ...
	 *
	 * @return true if the parameter is indeed in infix notation, and false otherwise.
	 **/
	public static boolean validateInfixOrder(String infixLiterals[]) {
		if (infixLiterals == null || infixLiterals.length == 0)
			return false;

		// NOTE: See top of class for reason on using Integers here rather than an enum
		Stack<Integer> stateStack = new Stack<>();
		stateStack.push(OPERAND_EXPECTED);

		for (String infixLiteral : infixLiterals) {

			if (infixLiteral == null || stateStack.empty())
				return false;

			switch (stateStack.pop()) {

				// Either of the operands are expected, or a subexpression in its place
				case OPERAND_EXPECTED:
					if (infixLiteral.equals("(")) {
						stateStack.push(CLOSING_PARENTHESIS_EXPECTED);
						stateStack.push(OPERAND_EXPECTED);
						stateStack.push(OPERATOR_EXPECTED);
						stateStack.push(OPERAND_EXPECTED);
					} else if (!isStringInteger(infixLiteral)) {
						return false;
					}
					break;

				// An operator is the next expected token
				case OPERATOR_EXPECTED:
					if (!VALID_OPERATORS.contains(infixLiteral))
						return false;
					break;

				// Else it must be a closing parenthesis
				default:
					if (!infixLiteral.equals(")"))
						return false;
					break;

			}
		}

		return stateStack.empty();
	}

	/**
	 * Validation method for postfix notation.
	 * Time complexity: O(n) for n elements in postfixLiterals.
	 * Justification: With a length of n, postfixLiterals is iterated just over once.
	 *
	 * This method uses a simple counting algorithm to check if there are enough operands
	 * provided before an operator for the expression to be valid. I don't believe it's
	 * possible to make a more simple validation method.
	 * An alternative would be to check if the first two elements are operands, the last
	 * element is an operator and there are n operands and n-1 operators.
	 *
	 * @param postfixLiterals : an array containing the string literals hopefully in postfix order.
	 * The method assumes that each of these literals can be one of:
	 * - "+", "-", "*", or "/"
	 * - or a valid string representation of an integer "0", "1" , "2", ..., "-1", "-2", ...
	 *
	 * @return true if the parameter is indeed in postfix notation, and false otherwise.
	 **/
	public static boolean validatePostfixOrder(String postfixLiterals[]) {
		if (postfixLiterals == null || postfixLiterals.length == 0)
			return false;

		int operandCount = 0;

		for (String postfixLiteral : postfixLiterals) {
			if (postfixLiteral == null)
				return false;
			if (isStringInteger(postfixLiteral))
				operandCount++;
			else if (!VALID_OPERATORS.contains(postfixLiteral) || --operandCount <= 0)
				return false;
		}

		return operandCount == 1;
	}


	//~ Evaluation  methods ..........................................................

	/**
	 * Evaluation method for infix notation.
	 * Time complexity: O(n)
	 * Justification: Both evaluatePostfixOrder and convertInfixToPostfix have a running time of O(n)
	 *
	 * This is simply evaluatePostfixOrder() but the expression is first converted into
	 * postfix notation using convertInfixToPostfix().
	 *
	 * @param infixLiterals : an array containing the string literals in infix order.
	 * The method assumes that each of these literals can be one of:
	 * - "+", "-", "*", or "/"
	 * - or a valid string representation of an integer.
	 *
	 * @return the integer result of evaluating the expression
	 **/
	public static int evaluateInfixOrder(String infixLiterals[]) {
		return evaluatePostfixOrder(convertInfixToPostfix(infixLiterals));
	}

	/**
	 * Evaluation method for postfix notation.
	 * Time complexity: O(n) for n elements in postfixLiterals
	 * Justification: With a length of n, postfixLiterals is iterated over just once.
	 *
	 * To evaluate a postfix expression, I have used a stack to hold operands until
	 * an operator is encountered. Then two operands are popped off the stack and used
	 * in the operation. The result is then pushed back onto the stack.
	 * After completing the expression, a single value will be left on the stack.
	 * This is the result and can be returned.
	 * This is a very common algorithm used to evaluate postfix expressions. It
	 * completes the expression in linear time, so I believe this is an optimal solution.
	 *
	 * @param postfixLiterals : an array containing the string literals in postfix order.
	 * The method assumes that each of these literals can be one of:
	 * - "+", "-", "*", or "/"
	 * - or a valid string representation of an integer.
	 *
	 * @return the integer result of evaluating the expression
	 **/
	public static int evaluatePostfixOrder(String postfixLiterals[]) {
		if (!validatePostfixOrder(postfixLiterals))
			return 0;

		Stack<Integer> operandStack = new Stack<>();

		for (String postfixLiteral : postfixLiterals) {
			if (isStringInteger(postfixLiteral)) {
				operandStack.push(Integer.parseInt(postfixLiteral));
			} else {
				int rhs = operandStack.pop();
				int lhs = operandStack.pop();
				// NOTE: WebCat code coverage does not like String switch statements,
				//       (due to the Java bytecode having double the branch count)
				//       so I've opted to use a traditional if-else block here
				//switch (postfixLiteral) {
				//	case "+": operandStack.push(lhs + rhs); break;
				//	case "-": operandStack.push(lhs - rhs); break;
				//	case "*": operandStack.push(lhs * rhs); break;
				//	case "/": operandStack.push(lhs / rhs); break;
				//}
				if (postfixLiteral.equals("+"))
					operandStack.push(lhs + rhs);
				else if (postfixLiteral.equals("-"))
					operandStack.push(lhs - rhs);
				else if (postfixLiteral.equals("*"))
					operandStack.push(lhs * rhs);
				// NOTE: This condition is always true because the expression
				//       must be valid, so we must appease WebCat like such...
				else //if (postfixLiteral.equals("/"))
					operandStack.push(lhs / rhs);
			}
		}
		
		return operandStack.pop();
	}


	//~ Conversion  methods ..........................................................

	/**
	 * Converts infix to postfix.
	 * Time complexity: O(n) for n elements in infixLiterals
	 * Justification: With a length of n, infixLiterals is iterated over just once.
	 *
	 * I used the 'Shunting-yard' algorithm to convert infix to postfix. This uses
	 * a single stack to hold operators and parenthesis. This implementation is
	 * quite simple as the expression is assumed to be fully parenthesized. This
	 * removes the need to check fore operator precedence or extra operators.
	 * I don't know of a better algorithm for converting infix to postfix.
	 *
	 * @param infixLiterals : an array containing the string literals in infix order.
	 * The method assumes that each of these literals can be one of:
	 * - "+", "-", "*", or "/"
	 * - or a valid string representation of an integer.
	 *
	 * @return the expression in postfix order.
	 **/
	public static String[] convertInfixToPostfix(String infixLiterals[]) {
		if (!validateInfixOrder(infixLiterals))
			return null;

		// A fully parenthesized infix expression of length n
		// has a postfix expression of length n / 2 + 1
		int nextPostfixLiterialIndex = 0;
		String[] postfixLiterals = new String[infixLiterals.length / 2 + 1];

		String latestOperator;
		Stack<String> operatorStack = new Stack<>();
		for (String infixLiterial : infixLiterals) {

			// Add numbers straight to the output array
			if (isStringInteger(infixLiterial))
				postfixLiterals[nextPostfixLiterialIndex++] = infixLiterial;

			// If "(", add to operator stack
			else if (infixLiterial.equals("("))
				operatorStack.push(infixLiterial);

			// If ")", pop off stack onto array until "(" or end reached
			else if (infixLiterial.equals(")"))
				// NOTE: No need to check if empty here, a valid infix expressions will always have a corresponding parenthesis
				while (!(latestOperator = operatorStack.pop()).equals("("))
					postfixLiterals[nextPostfixLiterialIndex++] = latestOperator;

			// Else it must be an operator
			else
				operatorStack.push(infixLiterial);
		}

		// Finish by popping off remaining operator stack
		// NOTE: This code would never run due to the input being fully parenthesized.
		//       The last parenthesis would thus pop off the entire stack.
		//while (!operatorStack.empty())
		//	postfixLiterals.add(operatorStack.pop());

		return postfixLiterals;
	}

	/**
	 * Converts postfix to infix.
	 * Time complexity: O(n^2) for n elements in postfixLiterals
	 * Justification: With a length of n, postfixLiterals is iterated over just once.
	 *                However, for each operator, two subarrays must be copied into
	 *                a new array. This cost can be reduced down to simply O(n*n) or O(n^2).
	 *
	 * It is easiest to convert postfix to infix when allowed to be fully parenthesized.
	 * To do so, you can maintain a stack of operands until an operator is reached.
	 * Then place the two most recent operands around the operator, wrap in parentheses
	 * and place the result back on the stack.
	 * However, the inputs and outputs in this implementation use arrays of strings. This
	 * requires careful array copying and manipulation when creating the new subexpression
	 * after an operator is reach.
	 * Due to this, each operator encountered requires a loop over data, resulting in
	 * a total running time of O(n^2).
	 *
	 * @param postfixLiterals : an array containing the string literals in postfix order.
	 * The method assumes that each of these literals can be one of:
	 * - "+", "-", "*", or "/"
	 * - or a valid string representation of an integer.
	 *
	 * @return the expression in infix order.
	 **/
	public static String[] convertPostfixToInfix(String postfixLiterals[]) {
		if (!validatePostfixOrder(postfixLiterals))
			return null;

		Stack<String[]> infixLiterals = new Stack<>();

		for (String postfixLiteral : postfixLiterals) {

			// Add any numbers to the stack
			if (isStringInteger(postfixLiteral)) {
				infixLiterals.push(new String[] { postfixLiteral });
			}

			// Add operators to stack between two highest stack operands
			else {
				String rhs[] = infixLiterals.pop();
				String lhs[] = infixLiterals.pop();

				String newSubexpression[] = new String[lhs.length + rhs.length + 3];     // { "(", rhs.clone(), operator, lhs.clone(), ")" }
				newSubexpression[0] = "(";                                               // opening parenthesis
				System.arraycopy(lhs, 0, newSubexpression, 1, lhs.length);               // lhs operand
				newSubexpression[lhs.length + 1] = postfixLiteral;                       // operator
				System.arraycopy(rhs, 0, newSubexpression, lhs.length + 2, rhs.length);  // rhs operand
				newSubexpression[lhs.length + rhs.length + 2] = ")";                     // closing parenthesis

				infixLiterals.push(newSubexpression);
			}
		}

		return infixLiterals.pop();
	}
}

/*

Data structures used:
 - java.lang.String
 - java.util.Stack

During development, I used the OpenJDK implementation.
Below, I will use their source code to determine each methods running time.

String methods

  .length()
    This method simply counts the characters in a String.
    It has a constant running time O(1) in OpenJDK.
    All implementations are the same, as Java strings are
    immutable and their length does not change.
    OpenJDK String.length(): https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/String.java#L674

  .charAt()
    Simply access the string and returns the character at a given index.
    It has a constant running time O(1) in OpenJDK.
    It's reasonable to expect most implementations have a constant character lookup time.
    OpenJDK String.charAt(): https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/String.java#L709

  .equals()
    Two strings are compared, where the shorter is iterated through entirely.
    This gives the method a linear running time O(n) for n characters.
    OpenJDK String.equals(): https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/String.java#L1020

  .contains()
    In OpenJDK, this uses the .indexOf() method internally.
    That method scans through a string, looking for a substring matching the target.
    Therefore it has a running time of O(nm), for n character source and m character target strings.
    Most implementations would have a similar running time, however some do achieve O(n).
    OpenJDK String.contains(): https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/String.java#L2053


Stack methods

  .push()
    A stack should have a constant insertion running time O(1).
    In OpenJDK, they extended Vector class and call Vector.addElement().
    This gives it a worst case running time of O(n), as the Stack needs
    to grow using Arrays.copyOf() after the first 10 elements are filled.
    To maintain academic accuracy, we will except OpenJDK's implementation here.
    OpenJDK Stack.push():  https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/Stack.java#L65

  .pop()
    Again, a stack should have a constant deletion running time O(1).
    However, in OpenJDK, they use Vector.removeElementAt() which calls
    System.arraycopy to shrink the vector. This has a running time of O(n).
    To maintain academic accuracy, we will except OpenJDK's implementation here.
    OpenJDK Stack.push(): https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/Stack.java#L79

  .empty()
    This has a constant running time O(1).
    In OpenJDK, this uses a call to Vector.size(), which is just a variable lookup.
    It's reasonable to expect other implementations to do the same
    OpenJDK Stack.empty(): https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/Stack.java#L111

*/

