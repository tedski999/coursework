import java.util.Scanner;

public class CheckDateFormat
{
	public static final int NUMBER_OF_MONTHS_IN_YEAR = 12;
	public static final int DAYS_IN_MOST_MONTHS = 31;
	public static final int DAYS_IN_OTHER_MONTHS = 30;
	public static final int DAYS_IN_FEBUARY = 28;
	public static final int DAYS_IN_FEBUARY_ON_LEAP_YEAR = 29;
	public static final int FEBUARY = 2;
	public static final int APRIL = 4;
	public static final int JUNE = 6;
	public static final int SEPTEMBER = 9;
	public static final int NOVEMBER = 11;

	public static void main(String[] args)
	{
		Scanner inputScanner = new Scanner(System.in);
		System.out.print("Please enter a day: ");
		int inputDay = parseValidIntFromScanner(inputScanner);
		System.out.print("Please enter a month: ");
		int inputMonth = parseValidIntFromScanner(inputScanner);
		System.out.print("Please enter a year: ");
		int inputYear = parseValidIntFromScanner(inputScanner);
		inputScanner.close();

		if (validDate(inputYear, inputMonth, inputDay))
			System.out.println("That's a valid date!");
		else
			System.out.println("That's not a valid date!");
	}

	private static int parseValidIntFromScanner(Scanner scanner)
	{
		boolean validInput = false;
		while (!validInput)
		{
			if (scanner.hasNextInt())
				validInput = true;
			System.out.print("That's not a number! Try again: ");
			scanner.next();
		}

		return scanner.nextInt();
	}

	private static boolean validDate(int year, int month, int day)
	{
		boolean isValidDate = true;
		if (day <= 0 || day > daysInMonth(month, year))
			isValidDate = false;
		if (month <= 0 || month > NUMBER_OF_MONTHS_IN_YEAR)
			isValidDate = false;
		return isValidDate;
	}

	private static int daysInMonth(int month, int year)
	{
		int numberOfDays = DAYS_IN_MOST_MONTHS;
		switch (month)
		{
		case APRIL:
		case JUNE:
		case SEPTEMBER:
		case NOVEMBER:
			numberOfDays = DAYS_IN_OTHER_MONTHS;
			break;

		case FEBUARY:
			if (isLeapYear(year))
				numberOfDays = DAYS_IN_FEBUARY_ON_LEAP_YEAR;
			else
				numberOfDays = DAYS_IN_FEBUARY;
			break;

		default:
			break;
		}

		return numberOfDays;
	}

	private static boolean isLeapYear(int year)
	{
		if (year % 4 == 0)
			if (year % 100 != 0 || year % 400 == 0)
				return true;
		return false;
	}
}
