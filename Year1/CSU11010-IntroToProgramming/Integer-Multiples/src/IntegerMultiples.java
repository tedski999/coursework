
import java.util.Scanner;

public class IntegerMultiples
{
	public static final String POSITIVE_INTEGER_PROMPT = "Please enter a positive integer: ";
	public static final String LIMIT_PROMPT = "Please enter the limit: ";
	public static final String MIN_VALUE_ERROR = "ERR: That value is too low!";
	public static final String INVALID_TYPE_ERROR = "ERR: That's not a number!";
	public static final String OUTPUT_PATTERN = "The multiples of %d (Up to %d) are 0";

	public static void main(String[] args)
	{
		Scanner inputScanner = new Scanner(System.in);
		int positiveInteger = parseInputToInt(inputScanner, POSITIVE_INTEGER_PROMPT, 1);
		int limit = parseInputToInt(inputScanner, LIMIT_PROMPT, positiveInteger);
		inputScanner.close();

		System.out.printf(OUTPUT_PATTERN, positiveInteger, limit);
		for (int i = 1; i < limit; i++)
			if (i % positiveInteger == 0)
				System.out.print(", " + i);
		System.out.println(".");
	}

	public static int parseInputToInt(Scanner scanner, String promptMsg, int minValue)
	{
		int returnInt = 0;

		while (returnInt < minValue)
		{
			System.out.print(promptMsg);
			if (!scanner.hasNextInt())
			{
				System.out.println(INVALID_TYPE_ERROR);
				scanner.next();
				continue;
			}
			returnInt = scanner.nextInt();
			if (returnInt < minValue)
				System.out.println(MIN_VALUE_ERROR);
		}

		return returnInt;
	}
}
