// CSU11010 - Introduction to Programming 1 - Practice Assignment 1
//
// USING A WHILE OR A DO-WHILE LOOP write a program to compute PI
// using the following equation:
// PI = 3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) - 4/(8*9*10) + ...
// Allow the user to specify the number of terms (5 terms are shown)
// to use in the computation. Each time around the loop only one
// extra term should be added to the estimate for PI.
//
// Ted Johnson - 11/10/19

import java.util.Scanner;

public class ComputePi
{
	public static final int MIN_NUMBER_OF_ITERATIONS = 1;
	public static final int STARTING_DIGIT = 3;
	public static final int MULTIPLIER = 4;

	public static void main(String[] args)
	{
		Scanner inputScanner = new Scanner(System.in);
		int numberOfIterations = -1;
		System.out.println("How many iterations should I use to compute Pi?");
		while (numberOfIterations < MIN_NUMBER_OF_ITERATIONS)
		{
			System.out.print(" > ");
			if (inputScanner.hasNextInt())
			{
				numberOfIterations = inputScanner.nextInt();
				if (numberOfIterations < MIN_NUMBER_OF_ITERATIONS)
					System.out.println("Please enter a number greater than 0!");
			}
			else
			{
				inputScanner.next();
				System.out.println("That's not a number!");
			}
		}
		inputScanner.close();

		int termsDone = 0;
		double estimateOfPi = STARTING_DIGIT;
		while (termsDone++ < numberOfIterations)
		{
			int doubleTermsDone = termsDone * 2;
			int numerator = (termsDone % 2 == 0) ? -MULTIPLIER : MULTIPLIER;
			int denominator = doubleTermsDone * (doubleTermsDone + 1) * (doubleTermsDone + 2);
			estimateOfPi += (double) numerator / denominator;
		}

		System.out.println("My estimate of pi is...");
		System.out.println(estimateOfPi);
	}
}
