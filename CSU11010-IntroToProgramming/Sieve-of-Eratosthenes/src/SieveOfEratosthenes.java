
/* SELF ASSESSMENT
   1.  createSequence:

    Did I use the correct method definition?
    Mark out of 5: 5
    Comment: The method takes an integer for the max value and returns an array of strings which are numbers from 2 to the max value.

    Did I create an array of size n (passed as the parameter) and initialise it?
    Mark out of 5: 5
    Comment: The array is declared first and then each value is set in the for loop.

    Did I return the correct item?
    Mark out of 5: 5
    Comment: The method returns the correctly generated sequence.

   2.  crossOutMultiples

    Did I use the correct method definition?
    Mark out of 5: 5
    Comment: The method takes a sequence (as an array of strings) and a number for the base multiple.

    Did I ensure the parameters are not null and one of them is a valid index into the array
    Mark out of 2: 2
    Comment: There is an if statement which checks if the array is either null or empty before the code accesses any values in it.

    Did I loop through the array using the correct multiple?
    Mark out of 5: 5
    Comment: The array was looped through using a for loop. Each higher multiple of the base number is calculated by continuously adding the base number to itself.

    Did I cross out correct items in the array that were not already crossed out?
    Mark out of 3: 3
    Comment: When a higher multiple that is not crossed out is found, it is crossed out by prepending and appending square brackets.

   3.  sieve

    Did I have the correct function definition?
    Mark out of 5: 5
    Comment: The method takes a number for the max value of the sequence and returns an array of strings for the completed sequence.

    Did I make calls to other methods?
    Mark out of 5: 5
    Comment: The other implemented methods are called to from withing this method correctly.

    Did I return an array with all non-prime numbers are crossed out?
    Mark out of 2: 2
    Comment: The array of strings is returned with non-prime numbers placed within square brackets to signify they have been crossed out.

   4.  sequenceToString

    Did I have the correct function definition?
    Mark out of 5: 5
    Comment: The method takes an array of strings (the sequence) and returns a string (the formatted sequence as a string)

    Did I ensure the parameter to be used is not null?
    Mark out of 3: 3
    Comment: The method returns an empty string if the sequence is null or empty.

    Did I Loop through the array updating the String variable with the non-crossed out numbers and the crossed numbers in brackets?
    Mark out of 10: 10
    Comment: As the crossed out numbers are already represented with square brackets, I was able to simply append all the elements in the array with some formatting.

   5.  nonCrossedOutSubseqToString

    Did I have the correct function definition?
    Mark out of 5: 5
    Comment: The method takes an array of strings (the sequence) and returns a string (the formatted sequence as a string)

    Did I ensure the parameter to be used is not null?
    Mark out of 3: 3
    Comment: The method returns an empty string if the sequence is null or empty.

    Did I loop through the array updating the String variable with just the non-crossed out numbers?
    Mark out of 5: 5
    Comment: Each element is check if it is crossed out. If it isn't, it is appended to the string variable with some formatting.

   6.  main

    Did I ask  the user for input n and handles input errors?
    Mark out of 5: 5
    Comments: The utility method parseIntFromScanner repeatedly reads user input until a valid value is provided.

    Did I make calls to other methods (at least one)?
    Mark out of 5: 5
    Comment: All of the Sieve Of Eratosthenes algorithm is located within the sieve method which is called in main. There is also a call to the parseIntFromScanner and nonCrossedOutSubseqToString methods.

    Did I print the output as shown in the question?
    Mark out of 5: 5
    Comment: The example output is followed closely, with only readability improvements. Note that some of the print calls are located within the algorithm (not in main).

   7.  Overall

    Is my code indented correctly?
    Mark out of 4: 4
    Comments: The code is properly indented with 4-spaced tabs.

    Do my variable names make sense?
    Mark out of 4: 4
    Comments: Variable names are concise, self-descriptive and reflect their purpose.

    Do my variable names, method names and class name follow the Java coding standard?
    Mark out of 4: 4
    Comments: Local variables are lowerCamelCase, constants are UPPERCASE_WITH_UNDERSCORES, method names are lowerCamelCase and class names are CamelCase.

    Total Mark out of 100 (Add all the previous marks): 100
*/


import java.util.Scanner;


public class SieveOfEratosthenes
{
	private static final int MIN_VALUE = 2;

	// Performs an implementation of the Sieve of Eratosthenes on a sequence of number up to and including maxValue
	public static String[] sieve(int maxValue)
	{
		String[] sequence = createSequence(maxValue);
		System.out.println("Generated sequence: " + sequenceToString(sequence));

		for (int baseNumber = MIN_VALUE; baseNumber < (maxValue / 2) + 1; baseNumber++)
		{
			if (!sequence[baseNumber - MIN_VALUE].startsWith("["))
			{
				crossOutHigherMultiples(sequence, baseNumber);
				System.out.println("Crossed out higher multiples of " + baseNumber + ": " + sequenceToString(sequence));
			}
		}

		return sequence;
	}

	// Generates and returns a sequence of values from 2 up to and including maxValue
	private static String[] createSequence(int maxValue)
	{
		String[] sequence = new String[maxValue - 1];
		for (int index = 0; index < sequence.length; index++)
			sequence[index] = String.valueOf(index + MIN_VALUE);
		return sequence;
	}

	// Crosses out all the higher multiples of baseNumber in the sequence
	private static void crossOutHigherMultiples(String[] sequence, int baseNumber)
	{
		if (sequence == null || sequence.length == 0)
			return;

		for (int currentMultiple = baseNumber * 2; currentMultiple < sequence.length + MIN_VALUE; currentMultiple += baseNumber)
			if (!sequence[currentMultiple - MIN_VALUE].startsWith("["))
				sequence[currentMultiple - MIN_VALUE] = "[" + sequence[currentMultiple - MIN_VALUE] + "]";
	}

	// Return a formatted string of the sequence
	private static String sequenceToString(String[] sequence)
	{
		if (sequence == null || sequence.length == 0)
			return "";

		StringBuilder sequenceString = new StringBuilder(sequence[0]);
		for (int index = 1; index < sequence.length; index++)
			sequenceString.append(", ").append(sequence[index]);
		sequenceString.append(".");
		return sequenceString.toString();
	}

	// Return a formatted string containing the remaining values in the sequence
	private static String nonCrossedOutSubseqToString(String[] sequence)
	{
		if (sequence == null || sequence.length == 0)
			return "";

		StringBuilder sequenceString = new StringBuilder(sequence[0]);
		for (int index = 1; index < sequence.length; index++)
			if (!sequence[index].startsWith("["))
				sequenceString.append(", ").append(sequence[index]);
		sequenceString.append(".");
		return sequenceString.toString();
	}

	// Repeatedly read user input until a valid value is provided
	private static int parseIntFromScanner(Scanner scanner)
	{
		int inputValue = 0;

		boolean isInputting = true;
		while (isInputting)
		{
			System.out.print("> ");
			if (scanner.hasNextInt())
			{
				inputValue = scanner.nextInt();
				if (inputValue > MIN_VALUE)
					isInputting = false;
				else
					System.out.println("The value must be greater than " + MIN_VALUE + "!");
			}
			else
			{
				scanner.next();
				System.out.println("That is not a valid integer!");
			}
		}

		return inputValue;
	}

	// Driver code for class testing purposes
	public static void main(String[] args)
	{
		System.out.println("Please enter a integer greater than " + MIN_VALUE + ":");
		Scanner inputScanner = new Scanner(System.in);
		int inputValue = parseIntFromScanner(inputScanner);
		inputScanner.close();

		String[] sequence = sieve(inputValue);

		System.out.print("Finished sequence: ");
		System.out.println(nonCrossedOutSubseqToString(sequence));
	}
}
