// CSU11010 - Introduction to Programming 1 - Assignment 1
//
// Write a Java program which computes the monthly disposable income
// of the user and also computes what percentage of their after
// tax income is disposable. For simplicity let's assume that
// all income is taxed at 35%.
// You must ask the user for their monthly gross income (i.e. before
// tax is taken), and their monthly costs of accommodation, travel
// and food, do some computations and then tell the user the required
// results.
//
// Ted Johnson - 18/09/19

/*	SELF ASSESSMENT
	1. Did I use appropriate CONSTANTS instead of numbers within the code?
		Mark out of 10: 10
		Comment: INCOME_TAX is set to 35% as stated in the assignment.

	2. Did I use easy-to-understand, meaningful CONSTANT names?
		Mark out of 5: 5
		Comment: INCOME_TAX represents exactly what it is called.

	3. Did I format the CONSTANT names properly (in UPPERCASE)?
		Mark out of 5: 5
		Comment: Yes, as constants should be ALL_CAPTIALS_WITH_UNDERSCORES.

	4. Did I use easy-to-understand meaningful variable names?
		Mark out of 10: 10
		Comment: Each variable name describes exactly what value it should hold.

	5. Did I format the variable names properly (in lowerCamelCase)?
		Mark out of 10: 10
		Comment: All variables have lower camel case formatting.

	6. Did I indent the code appropriately?
		Mark out of 10: 10
		Comment: Yes, using four-spaced tabs.

	7. Did I read the input correctly from the user using appropriate question(s)?
		Mark out of 10: 10
		Comment: Input is parsed using a Scanner, with questions preceding user input.
				 The program does not sanatise input at all, which is obviously a major
				 problem, but I believe that that is outside the scope of this assignment.

	8. Did I compute the disposable income correctly?
		Mark out of 10: 10
		Comment: I calculated the disposable income by applying income tax to the inputted
		         monthly income, and then subtracting the sum of all inputted costs.

	9. Did I compute the disposable income percentage correctly?
		Mark out of 10: 10
		Comment: The percentage can easily be calculated by dividing the disposable income
				 by the inputted monthly income. This should give a value between 0 to 1,
				 so to format the percetage when printing it, we simply multiply by 100.

	10. Did I output the correct answer in the correct format (as shown in the examples)?
		Mark out of 10: 10
		Comment: I used printf() to output the calculated values in the given format. I
				 was able to specify precision levels to remove trailing numbers. I also
				 used the % escape symbol to print the percentage sign with printf().

	11. How well did I complete this self-assessment?
		Mark out of 10: 10
		Comment: I think I spent more time writing about what I did then actually making it.

  Total Mark out of 100 (Add all the previous marks): 100
*/

import java.util.Scanner;

public class DisposableIncome {

	public static final double INCOME_TAX = 0.35;

	public static void main(String[] args) {

		System.out.println("\nEnter the following values to calculate your monthly disposable income...");

		Scanner inputScanner = new Scanner(System.in);
		System.out.print("Monthly income before tax:     €");
		double grossIncome = inputScanner.nextDouble();
		System.out.print("Accommodation costs per month: €");
		double accommodationCosts = inputScanner.nextDouble();
		System.out.print("Commute costs per month:       €");
		double commuteCosts = inputScanner.nextDouble();
		System.out.print("Food costs per month:          €");
		double foodCosts = inputScanner.nextDouble();
		inputScanner.close();

		double netIncome = grossIncome * (1 - INCOME_TAX);
		double totalCosts = accommodationCosts + commuteCosts + foodCosts;
		double disposableIncome = netIncome - totalCosts;
		double percentageOfSalary = (disposableIncome / grossIncome);

		System.out.printf("\nYour monthly disposable income is €%.2f ", disposableIncome);
		System.out.printf("which is %.0f%% of your salary.%n", percentageOfSalary * 100);
	}
}
