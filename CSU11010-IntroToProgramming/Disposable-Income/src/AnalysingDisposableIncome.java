// CSU11010 - Introduction to Programming 1 - Assignment 2
//
// Extend your program from last week and provide some analysis for
// the user regarding their disposable income.  Assuming that the
// average monthly disposable income is 500.00 per month tell the
// user if their disposable income if much more than the average
// (i.e. greater than 50% more than the average), more than the
// average, exactly the average, less than the average or much less
// than the average (i.e. less than 50% of the average).
// If the user has no disposable income (i.e. less than or equal to 0.00)
// just tell them that rather than doing the comparison.
//
// Ted Johnson - 27/09/19

/*	SELF ASSESSMENT
	1. Did I use easy-to-understand meaningful variable and CONSTANT names?
		Mark out of 10: 10
		Comment: Each variable and constant have descriptive names so it's easy to understand their purpose.
	2. Did I format the variable and CONSTANT names properly (in lowerCamelCase and UPPERCASE_WITH_UNDERSCORES)?
		Mark out of 10: 10
		Comment: Variables are formatted in lowerCamelCase and constants are UPPERCASE_WITH_UNDERSCORES.
	3. Did I indent the code appropriately?
		Mark out of 10: 10
		Comment: Yes, using four-spaced tabs.
	4. Did I read the input correctly from the user using appropriate questions?
		Mark out of 15: 15
		Comment: Input is stored as doubles in their relevant variables. The questions are clear about what the user has to input.
	5. Did I compute the disposable income and disposable income percentage correctly, and output it in the correct format?
		Mark out of 15: 15
		Comment:  I calculated the disposable income by applying income tax to the inputted monthly income, and then subtracting
					the sum of all inputted costs.
	6. Did I use an appropriate series of if statements to generate the income analysis to the user?
		Mark out of 25: 25
		Comment: I split the ranges up into 5 options: 'No disposable', 'Very low', 'Low', 'Average', 'High' and 'Very high'.
					First, it checks if the value is lower, higher or equal to the average. Then it narrows it down even further
					to see if the value is lower/higher than 50% of the average. These if/else statements are inline only because
					there would have been as many lines dedicated to braces as there was relavent code!
					To reduce the number of if/else statements needed, I considered 'No disposable income' the default value if no
					other range was correct.
	7. Did I provide the correct output for each possibility in an easy to read format?
		Mark out of 10: 10
		Comment: I used printf() to output the variables using the formats stated in the constants. These formats include where to
					place the variables in the string, where there should be a new-line and how many significant figures the output needs.
	8. How well did I complete this self-assessment?
		Mark out of 5: 5
		Comment: Not much to say here, so I'll write a little about the code as a whole:
					While I didn't pollute the code with comments, there are a couple that simply state what the next block of code performs.
					Also, I used the Allman code-style because I personally prefer matching brace indenting.

   Total Mark out of 100 (Add all the previous marks): 100
*/

import java.util.Scanner;

public class AnalysingDisposableIncome
{
	public static final double INCOME_TAX = 0.35;
	public static final double AVERAGE_DISPOSABLE_INCOME = 500.0;

	public static final String INTRODUCTION = "Enter the following values to calculate your monthly disposable income...";
	public static final String GROSS_INCOME_QUESTION = "Monthly income before tax:     €";
	public static final String ACCOMMODATION_COSTS_QUESTION = "Accommodation costs per month: €";
	public static final String COMMUTE_COSTS_QUESTION = "Commute costs per month:       €";
	public static final String FOOD_COSTS_QUESTION = "Food costs per month:          €";

	public static final String VERY_HIGH_DISPOSABLE_INCOME_RESPONSE = "You have much more than the average disposable income per month.";
	public static final String HIGH_DISPOSABLE_INCOME_RESPONSE = "You have more than the average disposable income per month.";
	public static final String AVERAGE_DISPOSABLE_INCOME_RESPONSE = "You have the average disposable income per month.";
	public static final String LOW_DISPOSABLE_INCOME_RESPONSE = "You have less than the average disposable income per month.";
	public static final String VERY_LOW_DISPOSABLE_INCOME_RESPONSE = "You have much less than the average disposable income per month.";
	public static final String NO_DISPOSABLE_INCOME_RESPONSE = "You have no disposable income to spare!";

	public static final String DISPOSABLE_OUTPUT_FORMAT = "Your monthly disposable income is €%.2f ";
	public static final String PERCENTAGE_OUTPUT_FORMAT = "which is %.0f%% of your salary.%n";

	public static void main(String[] args)
	{
		// User input
		System.out.println(INTRODUCTION);
		Scanner inputScanner = new Scanner(System.in);
		System.out.print(GROSS_INCOME_QUESTION);
		double grossIncome = inputScanner.nextDouble();
		System.out.print(ACCOMMODATION_COSTS_QUESTION);
		double accommodationCosts = inputScanner.nextDouble();
		System.out.print(COMMUTE_COSTS_QUESTION);
		double commuteCosts = inputScanner.nextDouble();
		System.out.print(FOOD_COSTS_QUESTION);
		double foodCosts = inputScanner.nextDouble();
		inputScanner.close();

		// Calculating disposable income and percentage of salary
		double netIncome = grossIncome * (1 - INCOME_TAX);
		double totalCosts = accommodationCosts + commuteCosts + foodCosts;
		double disposableIncome = netIncome - totalCosts;
		double percentageOfSalary = (disposableIncome / grossIncome);

		// Analysing disposable income to determine correct response
		String comparisonResponse = NO_DISPOSABLE_INCOME_RESPONSE;
		if (disposableIncome > AVERAGE_DISPOSABLE_INCOME)
		{
			if (disposableIncome >= AVERAGE_DISPOSABLE_INCOME * 1.5)
				comparisonResponse = VERY_HIGH_DISPOSABLE_INCOME_RESPONSE;
			else
				comparisonResponse = HIGH_DISPOSABLE_INCOME_RESPONSE;
		}
		else if (disposableIncome == AVERAGE_DISPOSABLE_INCOME)
		{
			comparisonResponse = AVERAGE_DISPOSABLE_INCOME_RESPONSE;
		}
		else if (disposableIncome < AVERAGE_DISPOSABLE_INCOME)
		{
			if (disposableIncome <= AVERAGE_DISPOSABLE_INCOME * 0.5)
				comparisonResponse = VERY_LOW_DISPOSABLE_INCOME_RESPONSE;
			else
				comparisonResponse = LOW_DISPOSABLE_INCOME_RESPONSE;
		}

		// Print information back to the user
		System.out.println();
		System.out.printf(DISPOSABLE_OUTPUT_FORMAT, disposableIncome);
		System.out.printf(PERCENTAGE_OUTPUT_FORMAT, percentageOfSalary * 100);
		System.out.printf(comparisonResponse);
	}
}
