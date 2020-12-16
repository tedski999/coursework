import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import java.lang.reflect.Method;

//-------------------------------------------------------------------------
/**
 *  Test class for Arith
 *
 *  @version 16/12/20
 *
 *  @author Ted Johnson
 */

@RunWith(JUnit4.class)
public class ArithTest {

	@Test
	public void testInstantiation() {
		// Ensures superficial code coverage
		assertNotNull("Checking class instantiation", new Arith());
	}

	@Test
	public void testIsStringInteger() throws Exception {
		// NOTE: Reflective programming is disallowed by WebCat, method made public instead
		// Using Java reflections to access private method isStringInteger
		//Method isStringInteger = Arith.class.getDeclaredMethod("isStringInteger", String.class);
		//isStringInteger.setAccessible(true);

		assertEquals("Checking integer string validation for null expression",  false, Arith.isStringInteger(null));
		assertEquals("Checking integer string validation for empty expression", false, Arith.isStringInteger(""));
		assertEquals("Checking integer string validation for 3",                true,  Arith.isStringInteger("3"));
		assertEquals("Checking integer string validation for -6",               true,  Arith.isStringInteger("-6"));
		assertEquals("Checking integer string validation for a",                false, Arith.isStringInteger("a"));
		assertEquals("Checking integer string validation for 100x",             false, Arith.isStringInteger("100x"));
	}

	@Test
	public void testValidateInfixOrder() {
		assertEquals("Checking infix validation for null expression",  false, Arith.validateInfixOrder(null));
		assertEquals("Checking infix validation for empty expression", false, Arith.validateInfixOrder(new String[] {}));

		assertEquals("Checking infix validation for null operand",                 false, Arith.validateInfixOrder(new String[] { null }));
		assertEquals("Checking infix validation for empty operand",                false, Arith.validateInfixOrder(new String[] { "" }));
		assertEquals("Checking infix validation for single operand: 3",            true,  Arith.validateInfixOrder(new String[] { "3" }));
		assertEquals("Checking infix validation for single operand: -6",           true,  Arith.validateInfixOrder(new String[] { "-6" }));
		assertEquals("Checking infix validation for invalid single operand: a",    false, Arith.validateInfixOrder(new String[] { "a" }));
		assertEquals("Checking infix validation for invalid single operand: 100x", false, Arith.validateInfixOrder(new String[] { "100x" }));

		assertEquals("Checking infix validation for invalid expression: ( 3 )",   false, Arith.validateInfixOrder(new String[] { "(", "3", ")" }));
		assertEquals("Checking infix validation for invalid expression: +",       false, Arith.validateInfixOrder(new String[] { "+" }));
		assertEquals("Checking infix validation for invalid expression: ( + )",   false, Arith.validateInfixOrder(new String[] { "(", "+", ")" }));
		assertEquals("Checking infix validation for invalid expression: ( 3 + )", false, Arith.validateInfixOrder(new String[] { "(", "3", "+", ")" }));
		assertEquals("Checking infix validation for invalid expression: ( + 3 )", false, Arith.validateInfixOrder(new String[] { "(", "+", "3", ")" }));
		assertEquals("Checking infix validation for invalid expression: (",       false, Arith.validateInfixOrder(new String[] { "(" }));
		assertEquals("Checking infix validation for invalid expression: )",       false, Arith.validateInfixOrder(new String[] { ")" }));
		assertEquals("Checking infix validation for invalid expression: ( )",     false, Arith.validateInfixOrder(new String[] { "(", ")" }));

		assertEquals("Checking infix validation for single operation expression: ( 1 + 2 )",         true,  Arith.validateInfixOrder(new String[] { "(", "1", "+", "2", ")" }));
		assertEquals("Checking infix validation for single operation expression: ( 1 - 2 )",         true,  Arith.validateInfixOrder(new String[] { "(", "1", "-", "2", ")" }));
		assertEquals("Checking infix validation for single operation expression: ( 1 * 2 )",         true,  Arith.validateInfixOrder(new String[] { "(", "1", "*", "2", ")" }));
		assertEquals("Checking infix validation for single operation expression: ( 1 / 2 )",         true,  Arith.validateInfixOrder(new String[] { "(", "1", "/", "2", ")" }));
		assertEquals("Checking infix validation for invalid single operation expression: ( 1 $ 2 )", false, Arith.validateInfixOrder(new String[] { "(", "1", "$", "2", ")" }));

		assertEquals("Checking infix validation for multi operation expression: ( ( 9 + 10 ) * 21 )",   true,  Arith.validateInfixOrder(new String[] { "(", "(", "9", "+", "10", ")", "*", "21", ")" }));
		assertEquals("Checking infix validation for multi operation expression: ( 4 / ( 8 * 4 ) )",     true,  Arith.validateInfixOrder(new String[] { "(", "4", "/", "(", "8", "*", "4", ")", ")" }));
		assertEquals("Checking infix validation for invalid multi operation expression: ( ( 7 - 1 ) )", false, Arith.validateInfixOrder(new String[] { "(", "(", "7", "-", "1", ")", ")" }));
		assertEquals("Checking infix validation for invalid multi operation expression: ( 4 / 8 * 4 )", false, Arith.validateInfixOrder(new String[] { "(", "4", "/", "8", "*", "4", ")" }));
		assertEquals("Checking infix validation for invalid multi operation expression: ( 10 + -20",    false, Arith.validateInfixOrder(new String[] { "(", "10", "+", "-20" }));
		assertEquals("Checking infix validation for invalid multi operation expression: 10 + -20 )",    false, Arith.validateInfixOrder(new String[] { "10", "+", "-20", ")" }));
		assertEquals("Checking infix validation for invalid multi operation expression: ( 3 + * 4 )",   false, Arith.validateInfixOrder(new String[] { "(", "3", "+", "*", "4", ")" }));

		assertEquals("Checking infix validation for complex multi operation expression: ((-33/2)*((9+9)-(8+8)))",           true,  Arith.validateInfixOrder(new String[] { "(", "(", "-33", "/", "2", ")", "*", "(", "(", "9", "+", "9", ")", "-", "(", "8", "+", "8", ")", ")", ")" }));
		assertEquals("Checking infix validation for complex multi operation expression: (((91-97)*3)-(8/(8-1)))",           true,  Arith.validateInfixOrder(new String[] { "(", "(", "(", "91", "-", "97", ")", "*", "3", ")", "-", "(", "8", "/", "(", "8", "-", "1", ")", ")", ")" }));
		assertEquals("Checking infix validation for invalid complex multi operation expression: ((((7*2)/2)*(10+10)+1)/2)", false, Arith.validateInfixOrder(new String[] { "(", "(", "(", "(", "7", "*", "2", ")", "/", "2", ")", "*", "(", "10", "*", "10", ")", "+", "1", ")", "/", "2", ")"  }));
		assertEquals("Checking infix validation for invalid complex multi operation expression: (((7*2)+3)/6))",            false, Arith.validateInfixOrder(new String[] { "(", "(", "(", "7", "*", "2", ")", "+", "3", ")", "/", "6", ")", ")" }));
	}

	@Test
	public void testValidatePostfixOrder() {
		assertEquals("Checking postfix validation for null expression",  false, Arith.validatePostfixOrder(null));
		assertEquals("Checking postfix validation for empty expression", false, Arith.validatePostfixOrder(new String[] {}));

		assertEquals("Checking postfix validation for null operand",                 false, Arith.validatePostfixOrder(new String[] { null }));
		assertEquals("Checking postfix validation for empty operand",                false, Arith.validatePostfixOrder(new String[] { "" }));
		assertEquals("Checking postfix validation for single operand: 3",            true,  Arith.validatePostfixOrder(new String[] { "3" }));
		assertEquals("Checking postfix validation for single operand: -6",           true,  Arith.validatePostfixOrder(new String[] { "-6" }));
		assertEquals("Checking postfix validation for invalid single operand: a",    false, Arith.validatePostfixOrder(new String[] { "a" }));
		assertEquals("Checking postfix validation for invalid single operand: 100x", false, Arith.validatePostfixOrder(new String[] { "100x" }));

		assertEquals("Checking postfix validation for invalid expression: +",   false, Arith.validatePostfixOrder(new String[] { "+" }));
		assertEquals("Checking postfix validation for invalid expression: 3 3", false, Arith.validatePostfixOrder(new String[] { "3", "3" }));
		assertEquals("Checking postfix validation for invalid expression: 3 +", false, Arith.validatePostfixOrder(new String[] { "3", "+" }));
		assertEquals("Checking postfix validation for invalid expression: + 3", false, Arith.validatePostfixOrder(new String[] { "+", "3" }));

		assertEquals("Checking postfix validation for single operation expression: 1 2 +",         true,  Arith.validatePostfixOrder(new String[] { "1", "2", "+" }));
		assertEquals("Checking postfix validation for single operation expression: 1 2 -",         true,  Arith.validatePostfixOrder(new String[] { "1", "2", "-" }));
		assertEquals("Checking postfix validation for single operation expression: 1 2 *",         true,  Arith.validatePostfixOrder(new String[] { "1", "2", "*" }));
		assertEquals("Checking postfix validation for single operation expression: 1 2 /",         true,  Arith.validatePostfixOrder(new String[] { "1", "2", "/" }));
		assertEquals("Checking postfix validation for invalid single operation expression: 1 2 $", false, Arith.validatePostfixOrder(new String[] { "1", "2", "$" }));

		assertEquals("Checking postfix validation for multi operation expression: 9 10 + 21 *",      true,  Arith.validatePostfixOrder(new String[] { "9", "10", "+", "21", "*" }));
		assertEquals("Checking postfix validation for multi operation expression: 4 8 4 * /",        true,  Arith.validatePostfixOrder(new String[] { "4", "8", "4", "*", "/" }));
		assertEquals("Checking postfix validation for invalid multi operation expression: 4 8 4 *",  false, Arith.validatePostfixOrder(new String[] { "4", "8", "4", "*" }));
		assertEquals("Checking postfix validation for invalid multi operation expression: 10 + -20", false, Arith.validatePostfixOrder(new String[] { "10", "+", "-20" }));
		assertEquals("Checking postfix validation for invalid multi operation expression: 3 4 + *",  false, Arith.validatePostfixOrder(new String[] { "3", "4", "+", "*" }));

		assertEquals("Checking postfix validation for complex multi operation expression: -33 2 / 9 9 + 8 8 + - *",        true,  Arith.validatePostfixOrder(new String[] { "-33", "2", "/", "9", "9", "+", "8", "8", "+", "-", "*" }));
		assertEquals("Checking postfix validation for complex multi operation expression: 91 97 - 3 * 8 8 1 - / -",        true,  Arith.validatePostfixOrder(new String[] { "91", "97", "-", "3", "*", "8", "8", "1", "-", "/", "-" }));
		assertEquals("Checking postfix validation for invalid complex multi operation expression: 91 97 3 * 8 8 1 - / -",  false, Arith.validatePostfixOrder(new String[] { "91", "97", "3", "*", "8", "8", "1", "-", "/", "-" }));
	}

	@Test
	public void testEvaluateInfixOrder() {
		assertEquals("Checking infix evaluation for null expression",  0, Arith.evaluateInfixOrder(null));
		assertEquals("Checking infix evaluation for empty expression", 0, Arith.evaluateInfixOrder(new String[] {}));

		assertEquals("Checking infix evaluation for single operand: 3",          3, Arith.evaluateInfixOrder(new String[] { "3" }));
		assertEquals("Checking infix evaluation for single operand: -6",        -6, Arith.evaluateInfixOrder(new String[] { "-6" }));
		assertEquals("Checking infix evaluation for invalid single operand: a",  0, Arith.evaluateInfixOrder(new String[] { "a" }));

		assertEquals("Checking infix evaluation for multi operation expression: ( ( 9 + 10 ) * 21 )",   399, Arith.evaluateInfixOrder(new String[] { "(", "(", "9", "+", "10", ")", "*", "21", ")" }));
		assertEquals("Checking infix evaluation for multi operation expression: ( 400 / ( 8 * 4 ) )",    12, Arith.evaluateInfixOrder(new String[] { "(", "400", "/", "(", "8", "*", "4", ")", ")" }));
		assertEquals("Checking infix evaluation for invalid multi operation expression: ( ( 7 - 1 ) )",   0, Arith.evaluateInfixOrder(new String[] { "(", "(", "7", "-", "1", ")", ")" }));

		assertEquals("Checking infix evaluation for complex multi operation expression: ((-33/2)*((9+9)-(8+8)))",           -32, Arith.evaluateInfixOrder(new String[] { "(", "(", "-33", "/", "2", ")", "*", "(", "(", "9", "+", "9", ")", "-", "(", "8", "+", "8", ")", ")", ")" }));
		assertEquals("Checking infix evaluation for complex multi operation expression: (((91-97)*3)-(8/(8-1)))",           -19, Arith.evaluateInfixOrder(new String[] { "(", "(", "(", "91", "-", "97", ")", "*", "3", ")", "-", "(", "8", "/", "(", "8", "-", "1", ")", ")", ")" }));
		assertEquals("Checking infix evaluation for invalid complex multi operation expression: ((((7*2)/2)*(10+10)+1)/2)",   0, Arith.evaluateInfixOrder(new String[] { "(", "(", "(", "(", "7", "*", "2", ")", "/", "2", ")", "*", "(", "10", "*", "10", ")", "+", "1", ")", "/", "2", ")"  }));
	}

	@Test
	public void testEvaluatePostfixOrder() {
		assertEquals("Checking postfix evaluation for null expression",  0, Arith.evaluatePostfixOrder(null));
		assertEquals("Checking postfix evaluation for empty expression", 0, Arith.evaluatePostfixOrder(new String[] {}));

		assertEquals("Checking postfix evaluation for single operand: 3",          3, Arith.evaluatePostfixOrder(new String[] { "3" }));
		assertEquals("Checking postfix evaluation for single operand: -6",        -6, Arith.evaluatePostfixOrder(new String[] { "-6" }));
		assertEquals("Checking postfix evaluation for invalid single operand: a",  0, Arith.evaluatePostfixOrder(new String[] { "a" }));

		assertEquals("Checking postfix evaluation for multi operation expression: 9 10 + 21 *",   399, Arith.evaluatePostfixOrder(new String[] { "9", "10", "+", "21", "*" }));
		assertEquals("Checking postfix evaluation for multi operation expression: 400 8 4 * /",    12, Arith.evaluatePostfixOrder(new String[] { "400", "8", "4", "*", "/" }));
		assertEquals("Checking postfix evaluation for invalid multi operation expression: 7 -",     0, Arith.evaluatePostfixOrder(new String[] { "7", "-" }));

		assertEquals("Checking postfix evaluation for complex multi operation expression: -33 2 / 9 9 + 8 8 + - *",           -32, Arith.evaluatePostfixOrder(new String[] { "-33", "2", "/", "9", "9", "+", "8", "8", "+", "-", "*" }));
		assertEquals("Checking postfix evaluation for complex multi operation expression: 91 97 - 3 * 8 8 1 - / -",           -19, Arith.evaluatePostfixOrder(new String[] { "91", "97", "-", "3", "*", "8", "8", "1", "-", "/", "-" }));
		assertEquals("Checking postfix evaluation for invalid complex multi operation expression: 7 2 * 2 / 10 10 * + 2 / *",   0, Arith.evaluatePostfixOrder(new String[] { "7", "2", "*", "2", "/", "10", "10", "*", "+", "2", "/", "*" }));
	}

	@Test
	public void testConvertInfixToPostfix() {
		assertArrayEquals("Checking infix to postfix for null expression",  null, Arith.convertInfixToPostfix(null));
		assertArrayEquals("Checking infix to postfix for empty expression", null, Arith.convertInfixToPostfix(new String[] {}));

		assertArrayEquals("Checking infix to postfix for single operand expression: 3",         new String[] { "3" },  Arith.convertInfixToPostfix(new String[] { "3" }));
		assertArrayEquals("Checking infix to postfix for single operand expression: -6",        new String[] { "-6" }, Arith.convertInfixToPostfix(new String[] { "-6" }));
		assertArrayEquals("Checking infix to postfix for invalid single operand expression: a", null,                  Arith.convertInfixToPostfix(new String[] { "a" }));

		assertArrayEquals("Checking infix to postfix for single operation expression: ( 1 + 2 )",         new String[] { "1", "2", "+" }, Arith.convertInfixToPostfix(new String[] { "(", "1", "+", "2", ")" }));
		assertArrayEquals("Checking infix to postfix for single operation expression: ( 1 - 2 )",         new String[] { "1", "2", "-" }, Arith.convertInfixToPostfix(new String[] { "(", "1", "-", "2", ")" }));
		assertArrayEquals("Checking infix to postfix for single operation expression: ( 1 * 2 )",         new String[] { "1", "2", "*" }, Arith.convertInfixToPostfix(new String[] { "(", "1", "*", "2", ")" }));
		assertArrayEquals("Checking infix to postfix for single operation expression: ( 1 / 2 )",         new String[] { "1", "2", "/" }, Arith.convertInfixToPostfix(new String[] { "(", "1", "/", "2", ")" }));
		assertArrayEquals("Checking infix to postfix for invalid single operation expression: ( 1 $ 2 )", null,                          Arith.convertInfixToPostfix(new String[] { "(", "1", "$", "2", ")" }));

		assertArrayEquals("Checking infix to postfix for multi operation expression: ( ( 9 + 10 ) * 21 )", new String[] { "9", "10", "+", "21", "*" }, Arith.convertInfixToPostfix(new String[] { "(", "(", "9", "+", "10", ")", "*", "21", ")" }));
		assertArrayEquals("Checking infix to postfix for multi operation expression: ( 400 / ( 8 * 4 ) )", new String[] { "400", "8", "4", "*", "/" }, Arith.convertInfixToPostfix(new String[] { "(", "400", "/", "(", "8", "*", "4", ")", ")" }));
		assertArrayEquals("Checking infix to postfix for multi operation expression: ( ( 7 - 1 ) )",       null,                                       Arith.convertInfixToPostfix(new String[] { "(", "(", "7", "-", "1", ")", ")" }));

		assertArrayEquals("Checking infix to postfix for complex multi operation expression: ((-33/2)*((9+9)-(8+8)))",           new String[] { "-33", "2", "/", "9", "9", "+", "8", "8", "+", "-", "*" }, Arith.convertInfixToPostfix(new String[] { "(", "(", "-33", "/", "2", ")", "*", "(", "(", "9", "+", "9", ")", "-", "(", "8", "+", "8", ")", ")", ")" }));
		assertArrayEquals("Checking infix to postfix for complex multi operation expression: (((91-97)*3)-(8/(8-1)))",           new String[] { "91", "97", "-", "3", "*", "8", "8", "1", "-", "/", "-" }, Arith.convertInfixToPostfix(new String[] { "(", "(", "(", "91", "-", "97", ")", "*", "3", ")", "-", "(", "8", "/", "(", "8", "-", "1", ")", ")", ")" }));
		assertArrayEquals("Checking infix to postfix for invalid complex multi operation expression: ((((7*2)/2)*(10+10)+1)/2)", null,                                                                     Arith.convertInfixToPostfix(new String[] { "(", "(", "(", "(", "7", "*", "2", ")", "/", "2", ")", "*", "(", "10", "*", "10", ")", "+", "1", ")", "/", "2", ")"  }));
	}

	@Test
	public void testConvertPostfixToInfix() {
		assertArrayEquals("Checking postfix to infix for null expression",  null, Arith.convertPostfixToInfix(null));
		assertArrayEquals("Checking postfix to infix for empty expression", null, Arith.convertPostfixToInfix(new String[] {}));

		assertArrayEquals("Checking postfix to infix for single operand expression: 3",         new String[] { "3" },  Arith.convertPostfixToInfix(new String[] { "3" }));
		assertArrayEquals("Checking postfix to infix for single operand expression: -6",        new String[] { "-6" }, Arith.convertPostfixToInfix(new String[] { "-6" }));
		assertArrayEquals("Checking postfix to infix for invalid single operand expression: a", null,                  Arith.convertPostfixToInfix(new String[] { "a" }));

		assertArrayEquals("Checking postfix to infix for single operation expression: 1 2 +",         new String[] { "(", "1", "+", "2", ")" }, Arith.convertPostfixToInfix(new String[] { "1", "2", "+" }));
		assertArrayEquals("Checking postfix to infix for single operation expression: 1 2 -",         new String[] { "(", "1", "-", "2", ")" }, Arith.convertPostfixToInfix(new String[] { "1", "2", "-" }));
		assertArrayEquals("Checking postfix to infix for single operation expression: 1 2 *",         new String[] { "(", "1", "*", "2", ")" }, Arith.convertPostfixToInfix(new String[] { "1", "2", "*" }));
		assertArrayEquals("Checking postfix to infix for single operation expression: 1 2 /",         new String[] { "(", "1", "/", "2", ")" }, Arith.convertPostfixToInfix(new String[] { "1", "2", "/" }));
		assertArrayEquals("Checking postfix to infix for invalid single operation expression: 1 2 $", null,                                     Arith.convertPostfixToInfix(new String[] { "1", "2", "$" }));

		assertArrayEquals("Checking postfix to infix for multi operation expression: 9 10 + 21 *", new String[] { "(", "(", "9", "+", "10", ")", "*", "21", ")" }, Arith.convertPostfixToInfix(new String[] { "9", "10", "+", "21", "*" }));
		assertArrayEquals("Checking postfix to infix for multi operation expression: 400 8 4 * /", new String[] { "(", "400", "/", "(", "8", "*", "4", ")", ")" }, Arith.convertPostfixToInfix(new String[] { "400", "8", "4", "*", "/" }));
		assertArrayEquals("Checking postfix to infix for invalid multi operation expression: 7 -", null,                                                           Arith.convertPostfixToInfix(new String[] { "7", "-" }));

		assertArrayEquals("Checking postfix to infix for complex multi operation expression: -33 2 / 9 9 + 8 8 + - *",           new String[] { "(", "(", "-33", "/", "2", ")", "*", "(", "(", "9", "+", "9", ")", "-", "(", "8", "+", "8", ")", ")", ")" }, Arith.convertPostfixToInfix(new String[] { "-33", "2", "/", "9", "9", "+", "8", "8", "+", "-", "*" }));
		assertArrayEquals("Checking postfix to infix for complex multi operation expression: 91 97 - 3 * 8 8 1 - / -",           new String[] { "(", "(", "(", "91", "-", "97", ")", "*", "3", ")", "-", "(", "8", "/", "(", "8", "-", "1", ")", ")", ")" }, Arith.convertPostfixToInfix(new String[] { "91", "97", "-", "3", "*", "8", "8", "1", "-", "/", "-" }));
		assertArrayEquals("Checking postfix to infix for invalid complex multi operation expression: 7 2 * 2 / 10 10 * + 2 / *", null,                                                                                                                       Arith.convertPostfixToInfix(new String[] { "7", "2", "*", "2", "/", "10", "10", "*", "+", "2", "/", "*" }));
	}
}

