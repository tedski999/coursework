// CSU11010 - Assignment 7
//
// Write a Java program which repeatedly takes the details of a mortgage from the user (as shown below)
// and calculates the required monthly repayment. In addition ask the user how much they can afford to
// pay per month and calculate how long it would take to repay the mortgage (again as shown below).
// After this ask them if they want to try again. You may assume that the mortgage is 20 years in
// duration.
// As part of your program you must write and use the following functions:
// - A readDoubleFromUser function which takes a user prompt (i.e. the question that the user should
//   be asked) and returns a double (which has been entered by the user).  Note that this routine must
//   deal with all possible input from the user and must keep repeating the request until the user
//   provides a double.
// - A calculateMonthlyRepayment function which takes the mortgage amount (Principal), the length of
//   the mortgage in years (Duration) and the annual interest rate (APR), and returns the amount that
//   would have to be repaid monthly.
//   Monthly Repayment = Principal * (APR/12/100) / (1 - (1+APR/12/100)^(-Duration*12))
// - A calculateMonthsToRepayMortgage function which takes the mortgage amount, the amount that will
//   be repaid monthly and the annual interest rate, and returns the number of months that it will
//   take to repay the mortgage.  You may assume that at the end of each month the mortgage amount is
//   increased based on the monthly interest rate (which is the annual interest rate divided by twelve)
//   and is decreased at the same time by the monthly repayment amount.
//
// Ted Johnson - 15/11/19

/* SELF ASSESSMENT

1. Did I use easy-to-understand, meaningful, properly formatted, variable names and CONSTANTS?
	Mark out of 10: 10
	Comment: Variable names are lowerCamelCase and constant names are UPPERCASE_WITH_UNDERSCORES. Their names are self-descriptive.

2. Did I indent the code appropriately?
	Mark out of 5: 5
	Comment: The code is properly indented with 4-spaced tabs.

3. Did I implement the mainline correctly with a loop which continues using the user says "no" ?
	Mark out of 10: 10
	Comment: The program uses while loop which continues until isUsingCalculator equals false, which is only set if the user enters 'no' when prompted if they would like to use the calculator again.

4. Did I obtain the relevant inputs from the user and produce the relevant outputs using the specified prompts & formats ?
	Mark out of 10: 10
	Comment: All the inputs are gathered using either the readDoubleFromUser or readYesOrNoFromUser functions. The prompts and output formats are the same as the sample output provided.

5. Did I implement the readDoubleFromUser function correctly and in a manner that can be easily understood (4 marks for function definition, 4 marks for function call and 12 marks for function implementation)?
	Mark out of 20: 20
	Comment: The readDoubleFromUser takes a string to prompt the user with and a scanner to read input from. It must take the scanner as a parameter because a local scanner must be closed at the end of the function,
	which actually closes the input stream as well (System.in)! Instead of making the Scanner object a global static, I created a scanner at the start of the main function and passed it to the input functions.
	The function loops until a valid input is provided, then returns the input as a double.

6. Did I implement the calculateMonthlyRepayment function correctly in a manner that can be easily understood (4 marks for function definition, 4 marks for function call and 12 marks for function implementation)?
	Mark out of 20: 20
	Comment: The calculateMonthlyRepayments function takes the principle of the mortgage, the mortgage duration and the annual interest rate. It computes the required payment every month in order to pay off the mortgage
	in the provided duration using the provided formula: Monthly Repayment = Principal * (APR/12/100) / (1 - (1+APR/12/100)^(-Duration*12)).

7. Did I implement the calculateMonthsToRepayMortgage function correctly in a manner that can be easilyunderstood (4 marks for function definition, 4 marks for function call and 12 marks for function implementation)?
	Mark out of 20: 20
	Comment: The calculateMonthsToRepayMortgage function takes the principle of the mortgage, the amount payed every month and the annual interest rate. It calculates the number of months required to pay off the loan
	using a while loop to simulate every month until the mortgage would be payed off. This function should not be called if the payment per month is not enough to cover the interest gained every month, as it would get
	stuck in an endless loop. To fix this, the program first checks if the provided payment per month is more than the amount gained by the interest on the first month.

8. How well did I complete this self-assessment?
	Mark out of 5: 5
	Comment: This self-assessment explains and makes reference to the code below in detail.

 Total Mark out of 100 (Add all the previous marks): 100

*/

import java.util.Scanner;

public class MortgageCalculator
{
	public static final double MORTGAGE_DURATION = 20.0;
	public static final int MONTHS_IN_YEAR = 12;

	public static void main(String[] args)
	{
		System.out.println("Welcome to the mortgage calculator.");

		Scanner inputScanner = new Scanner(System.in);
		boolean isUsingCalculator = true;

		while (isUsingCalculator)
		{
			// Compute monthly repayments from user input
			double principle = readDoubleFromUser("Please enter the mortgage amount: €", inputScanner);
			double apr = readDoubleFromUser("Please enter the annual interest rate (APR): ", inputScanner);
			double monthlyReplayments = calculateMonthlyRepayment(principle, MORTGAGE_DURATION, apr);
			System.out.printf("Assuming a 20 year term, the monthly repayments would be €%.2f\n", monthlyReplayments);

			// Compute time to pay off mortgage from user input if possible
			double paymentPerMonth = readDoubleFromUser("How much can you afford to pay per month? €", inputScanner);
			double minimumPaymentPerMonth = principle * (1 + (apr / MONTHS_IN_YEAR) / 100) - principle;
			if (paymentPerMonth >= minimumPaymentPerMonth)
			{
				int timeToReplayInMonths = calculateMonthsToRepayMortgage(principle, paymentPerMonth, apr);
				int monthsLeft = timeToReplayInMonths % MONTHS_IN_YEAR;
				int yearsLeft = (timeToReplayInMonths - monthsLeft) / MONTHS_IN_YEAR;

				if (yearsLeft == 0)
					System.out.printf("If you pay €%.2f per month your mortgage would be paid off in %d months.\n",
							paymentPerMonth, monthsLeft);
				else if (yearsLeft == 1)
					System.out.printf(
							"If you pay €%.2f per month your mortgage would be paid off in 1 year and %d months.\n",
							paymentPerMonth, monthsLeft);
				else
					System.out.printf(
							"If you pay €%.2f per month your mortgage would be paid off in %d years and %d months.\n",
							paymentPerMonth, yearsLeft, monthsLeft);
			}
			else
				System.out.printf(
						"€%.2f per month is not enough to pay off your mortgage! You need to pay at least €%.2f per month.\n",
						paymentPerMonth, minimumPaymentPerMonth);

			// Does the user want to use the calculator again?
			isUsingCalculator = readYesOrNoFromUser("Would you like to use the mortgage calculator again (yes/no)? ",
					inputScanner);
		}

		inputScanner.close();
		System.out.println("Goodbye!");
	}

	private static double readDoubleFromUser(String prompt, Scanner inputScanner)
	{
		boolean isInputting = true;
		double inputValue = 0;

		while (isInputting)
		{
			System.out.print(prompt);
			if (inputScanner.hasNextDouble())
			{
				inputValue = inputScanner.nextDouble();
				isInputting = false;
			}
			else
			{
				inputScanner.next();
				System.out.println("Please enter a valid number!");
			}
		}

		return inputValue;
	}

	private static boolean readYesOrNoFromUser(String prompt, Scanner inputScanner)
	{
		boolean isInputting = true;
		boolean inputValue = true;

		while (isInputting)
		{
			System.out.print(prompt);
			String input = inputScanner.next().toLowerCase();
			if (input.equals("no"))
			{
				isInputting = false;
				inputValue = false;
			}
			else if (input.equals("yes"))
				isInputting = false;
			else
				System.out.println("Please enter a valid input!");
		}

		return inputValue;
	}

	private static double calculateMonthlyRepayment(double principal, double duration, double apr)
	{
		double monthlyRepayment = principal * ((apr / MONTHS_IN_YEAR) / 100);
		monthlyRepayment /= 1 - Math.pow(1 + (apr / MONTHS_IN_YEAR) / 100, -duration * MONTHS_IN_YEAR);
		return monthlyRepayment;
	}

	private static int calculateMonthsToRepayMortgage(double principle, double paymentPerMonth, double apr)
	{
		int monthsToRepayMortgage = 0;
		double mortgageLeft = principle;

		while (mortgageLeft > 0)
		{
			monthsToRepayMortgage++;
			mortgageLeft *= 1 + (apr / MONTHS_IN_YEAR) / 100;
			mortgageLeft -= paymentPerMonth;
		}

		return monthsToRepayMortgage;
	}
}
