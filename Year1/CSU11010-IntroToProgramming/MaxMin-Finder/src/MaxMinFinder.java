import java.util.Scanner;

public class MaxMinFinder
{
	public static void main(String[] args)
	{
		Scanner inputScanner = new Scanner(System.in);
		System.out.print("Please enter your list of numbers (eg 1 3 8 12)\n > ");
		String inputString = inputScanner.nextLine();
		inputScanner.close();

		Scanner tokenScanner = new Scanner(inputString);
		boolean firstNumber = true;
		int maxNumber = 0;
		int minNumber = 0;
		while (tokenScanner.hasNext())
		{
			if (tokenScanner.hasNextInt())
			{
				int inputNumber = tokenScanner.nextInt();
				if (firstNumber || inputNumber > maxNumber)
					maxNumber = inputNumber;
				if (firstNumber || inputNumber < minNumber)
					minNumber = inputNumber;
				firstNumber = false;
			}
			else
			{
				System.out.println(tokenScanner.next() + " isn't a number, skipping...");
			}
		}
		tokenScanner.close();

		if (firstNumber)
		{
			System.out.println("I couldn't find any numbers in your list!");
		}
		else
		{
			System.out.println("Min number: " + minNumber);
			System.out.println("Max number: " + maxNumber);
		}
	}
}
