// CSU11010 - Introduction to Programming 1 - Assignment 5
//
// Write a program to produce the lyrics (all 12 verses) of the Twelve Days of Christmas.
// This is a cumulative song as each verse is build on top of the previous verses.
// You must
//   - have a loop for the verses,
//   - build up each of the verses within a loop (or switch statement),
//   - use at least one switch statement in your solution,
//   - only write the lines of text (or constants representing them) from the song once
//     in your program (e.g. you cannot have more than one statement in the entire program
//     which prints out "my true love sent to me")...
//
// Ted Johnson - 04/11/19

/* SELF ASSESSMENT
	1. Did I use appropriate CONSTANTS instead of numbers within the code?
		Mark out of 5: 5
		Comment: The number of the days are constants.
	2. Did I use easy-to-understand, meaningful CONSTANT names formatted correctly in UPPERCASE?
		Mark out of 5: 5
		Comment: Constants are ALL_CAPTIALS_WITH_UNDERSCORES, and their names are self-descriptive.
	3. Did I use easy-to-understand meaningful variable names formatted properly (in lowerCamelCase)?
		Mark out of 10: 10
		Comment: The only variables used are 'dayNumber' and 'dayString', which store the day in number and string form, respectively.
	4. Did I indent the code appropriately?
		Mark out of 10: 10
		Comment: The code is properly indented with four-spaced tabs.
	5. Did I use an appropriate loop (or loops) to produce the different verses?
		Mark out of 20: 20
		Comment: I used a for loop to iterate over all 12 days of the song to generate the lyrics.
	6. Did I use a switch to build up the verses?
		Mark out of 25: 25
		Comment: I used a switch statement in the printGiftLyrics() function. I used a if/else if/else statement in the printDayLyrics() function as it did not require the switch statements cascading conditions trait.
	7. Did I avoid duplication of code and of the lines which make up the verses (each line should be referred to in the code only once (or twice))?
		Mark out of 10: 10
		Comment: There is no duplicate code or lyrics used to generate the entirety of the song.
	8. Does the program produce the correct output?
		Mark out of 10: 10
		Comment: The twelve verses are outputed correctly. I considered having a new-line printed between each verse for clarity, however I did not want to deviate from the sample output.
	9. How well did I complete this self-assessment?
		Mark out of 5: 5
		Comment: The self-assessment makes relevant reference to the code provided.
Total Mark out of 100 (Add all the previous marks): 100
*/

public class TwelveDaysOfChristmas
{
	public static final int FIRST_DAY = 1;
	public static final int SECOND_DAY = 2;
	public static final int THIRD_DAY = 3;
	public static final int FOURTH_DAY = 4;
	public static final int FIFTH_DAY = 5;
	public static final int SIXTH_DAY = 6;
	public static final int SEVENTH_DAY = 7;
	public static final int EIGHTH_DAY = 8;
	public static final int NINTH_DAY = 9;
	public static final int TENTH_DAY = 10;
	public static final int ELEVENTH_DAY = 11;
	public static final int TWELFTH_DAY = 12;

	public static void main(String[] args)
	{
		for (int dayNumber = FIRST_DAY; dayNumber <= TWELFTH_DAY; dayNumber++)
		{
			printDayLyrics(dayNumber);
			printGiftLyrics(dayNumber);
		}
	}

	public static void printDayLyrics(int dayNumber)
	{
		String dayString = "first";

		if (dayNumber == SECOND_DAY)
			dayString = "second";
		else if (dayNumber == THIRD_DAY)
			dayString = "third";
		else if (dayNumber == FOURTH_DAY)
			dayString = "fourth";
		else if (dayNumber == FIFTH_DAY)
			dayString = "fifth";
		else if (dayNumber == SIXTH_DAY)
			dayString = "sixth";
		else if (dayNumber == SEVENTH_DAY)
			dayString = "seventh";
		else if (dayNumber == EIGHTH_DAY)
			dayString = "eighth";
		else if (dayNumber == NINTH_DAY)
			dayString = "third";
		else if (dayNumber == TENTH_DAY)
			dayString = "tenth";
		else if (dayNumber == ELEVENTH_DAY)
			dayString = "eleventh";
		else if (dayNumber == TWELFTH_DAY)
			dayString = "twelfth";

		System.out.println("On the " + dayString + " day of Christmas");
		System.out.println("my true love sent to me:");
	}

	public static void printGiftLyrics(int dayNumber)
	{
		switch (dayNumber)
		{
		case TWELFTH_DAY:
			System.out.println("12 Drummers Drumming,");
		case ELEVENTH_DAY:
			System.out.println("11 Pipers Piping,");
		case TENTH_DAY:
			System.out.println("10 Lords a Leaping,");
		case NINTH_DAY:
			System.out.println("9 Ladies Dancing,");
		case EIGHTH_DAY:
			System.out.println("8 Maids a Milking,");
		case SEVENTH_DAY:
			System.out.println("7 Swans a Swimming,");
		case SIXTH_DAY:
			System.out.println("6 Geese a Laying,");
		case FIFTH_DAY:
			System.out.println("5 Golden Rings,");
		case FOURTH_DAY:
			System.out.println("4 Calling Birds,");
		case THIRD_DAY:
			System.out.println("3 French Hens,");
		case SECOND_DAY:
			System.out.println("2 Turtle Doves,");
			System.out.println("and a Partridge in a Pear Tree.");
			break;
		case FIRST_DAY:
			System.out.println("A Partridge in a Pear Tree.");
			break;
		default:
			break;
		}
	}
}
