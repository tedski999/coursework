// CSU11010 - Introduction to Programming 1 - Practice Assignment 2
//
// Write a program which takes keeps taking in numbers from a user
// on the command line until the user enters "quit" or "exit".
// The user should be allowed to enter only one number each time
// and the system should compute the average and variance of all
// numbers entered so far each time.  To do this WITHOUT storing
// all the numbers we must use formulas which allow to compute the
// average and variance based only on the previous values (of the
// average and variance) together with the latest number entered.
//
// Ted Johnson - 11/10/19

/* SELF ASSESSMENT
	1. Did I use easy-to-understand meaningful variable names?
		Mark out of 10: 10
		Comment: Variables have self-descriptive names.
	2. Did I format the variable names properly (in lowerCamelCase)?
		Mark out of 5: 5
		Comment: Local variables are in lowerCamelCase. Constansts are UPPERCASE_WITH_UNDERSCORES
	3. Did I indent the code appropriately?
		Mark out of 10: 10
		Comment: I indented the code properly with 4-spaced tabs.
	4. Did I input the numbers one at a time from the command line?
		Mark out of 10: 10
		Comment: The user is prompt for each number, one at a time.
	5. Did I check the input to ensure that invalid input was handled appropriately?
		Mark out of 10: 10
		Comment: The input is check if it is a number or string, and then parsed to a relevent type. Invalid strings are rejected.
	6. Did I use an appropriate while or do-while loop to allow the user to enter numbers until they entered exit/quit?
		Mark out of 20: 20
		Comment: I use a while loop to collect user input until they entered 'exit' or 'quit' instead of another number.
	7. Did I implement the loop body correctly so that the average and variance were updated and output appropriately?
		Mark out of 30: 30
		Comment: The new average and variance value is calculated after each new number is entered. Their new values are printed to the console.
	8. How well did I complete this self-assessment?
		Mark out of 5: 5
		Comment: Self-assessment is the hardest part in assignments.
	Total Mark out of 100 (Add all the previous marks): 100
*/

import java.util.Scanner;

public class IncrementalStatistics
{

	public static void main(String[] args)
	{
		System.out.println("This program computes the average and variance of all numbers entered.");

		int inputCount = 0;
		double average = 0;
		double variance = 0;

		Scanner inputScanner = new Scanner(System.in);
		boolean isCollectingInput = true;
		while (isCollectingInput)
		{
			System.out.print("Enter a number (or type 'exit'): ");

			// Is the input a number?
			if (inputScanner.hasNextDouble())
			{
				double newInput = inputScanner.nextDouble();
				inputCount++;

				// New average calculation
				double oldAverage = average;
				average += (newInput - average) / inputCount;

				// New variance calculation
				double decayMultiplier = variance * (inputCount - 1);
				double differenceMultiplier = (newInput - oldAverage) * (newInput - average);
				variance = (decayMultiplier + differenceMultiplier) / inputCount;

				System.out.println("So far the average is " + average + " and the variance is " + variance);
			}

			// Is the input a valid command?
			else
			{
				String inputString = inputScanner.next();
				if (!(inputString.equals("exit") || inputString.equals("quit")))
					System.out.println("I don't understand that command!");
				else
					isCollectingInput = false;
			}
		}

		inputScanner.close();
		System.out.println("Goodbye.");
	}
}
