// CSU11010 - Introduction to Programming 1 - Assignment 6
//
// Rewrite the Twelve Days of Christmas program to again
// produce the lyrics (all 12 verses) but this time use
// functions. As part of your solution you must write and
// use the following functions:
// - A function called getVerse which takes the verse number
//   and returns a String containing the verse.
// - A function called getChristmasGift which takes a gift
//   number and returns a String containing the gift.
//   e.g. if passed a 5 it would return "five Golden Rings".
//   This function must use a switch statement.
// - A function called getOrdinalString which takes a number
//   and returns the relevant ordinal string.  e.g. if passed a
//   2 it would return "second". This function must use a series
//   of if statements or a series of conditional operators.
//
// Ted Johnson - 11/11/2019

/* SELF ASSESSMENT
	1. Did I use easy-to-understand meaningful, properly formatted, variable names and CONSTANTS?
		Mark out of 10: 10
		Comment: All variable and constant names are self-descriptive. Variable names are lowerCamelCase and constant names are UPPERCASE_WITH_UNDERSCORES.

	2. Did I implement the getVerse function correctly and in a manner that can be understood (5 marks for function definition, 5 marks for function call and 15 marks for function implementation)?
		Mark out of 25: 25
		Comment: The getVerse function takes an integer called 'verseNumber' and returns a string containing the entire verse.

	3. Did I implement the getChristmasGift function correctly using a switch statement and in a manner that can be understood (5 marks for function definition, 5 marks for function call and 15 marks for function implementation)?
		Mark out of 25: 25
		Comment: The getChristmasGift function takes a integer called 'giftNumber' and returns the correct string. It uses a switch statement to select this string.

	4. Did I implement the getOrdinalString function correctly using if or conditional operators and in a manner that can be understood (5 marks for function definition, 5 marks for function call and 15 marks for function implementation)?
		Mark out of 25: 25
		Comment: The getOrdinalString function takes a integer called 'number' and returns a string containing the ordinal version of its value.

	5. Does the program produce the output correctly?
		Mark out of 10: 10
		Comment: The program outputs all twelve verses of the song, with a new line in between each verse.

	6. How well did I complete this self-assessment?
		Mark out of 5: 5
		Comment: This self-assessment explains and makes reference to the code below correctly.

Total Mark out of 100 (Add all the previous marks): 100
*/

public class TwelveDaysOfChristmas_Functions
{
	public static final int FIRST_GIFT_NUMBER = 1;
	public static final int FIRST_VERSE_NUMBER = 1;
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
			System.out.print(getVerse(dayNumber));
	}

	private static String getVerse(int verseNumber)
	{
		String verse = "\nOn the " + getOrdinalString(verseNumber);
		verse += " day of Christmas,\nMy true love gave to me:\n";

		if (verseNumber == FIRST_VERSE_NUMBER)
			verse += "A " + getChristmasGift(FIRST_GIFT_NUMBER) + ".";
		else
		{
			for (int giftNumber = verseNumber; giftNumber > FIRST_GIFT_NUMBER; giftNumber--)
				verse += getChristmasGift(giftNumber) + ",\n";
			verse += "and a " + getChristmasGift(FIRST_GIFT_NUMBER) + ".";
		}

		return verse + "\n";
	}

	private static String getChristmasGift(int giftNumber)
	{
		String giftString = "Partridge in a Pear Tree";
		switch (giftNumber)
		{
		case TWELFTH_DAY:
			giftString = "12 Drummers Drumming";
			break;
		case ELEVENTH_DAY:
			giftString = "11 Pipers Piping";
			break;
		case TENTH_DAY:
			giftString = "10 Lords a Leaping";
			break;
		case NINTH_DAY:
			giftString = "9 Ladies Dancing";
			break;
		case EIGHTH_DAY:
			giftString = "8 Maids a Milking";
			break;
		case SEVENTH_DAY:
			giftString = "7 Swans a Swimming";
			break;
		case SIXTH_DAY:
			giftString = "6 Geese a Laying";
			break;
		case FIFTH_DAY:
			giftString = "5 Golden Rings";
			break;
		case FOURTH_DAY:
			giftString = "4 Calling Birds";
			break;
		case THIRD_DAY:
			giftString = "3 French Hens";
			break;
		case SECOND_DAY:
			giftString = "2 Turtle Doves";
			break;
		default:
			break;
		}

		return giftString;
	}

	private static String getOrdinalString(int number)
	{
		String ordinalString = "first";
		if (number == SECOND_DAY)
			ordinalString = "second";
		else if (number == THIRD_DAY)
			ordinalString = "third";
		else if (number == FOURTH_DAY)
			ordinalString = "fourth";
		else if (number == FIFTH_DAY)
			ordinalString = "fifth";
		else if (number == SIXTH_DAY)
			ordinalString = "sixth";
		else if (number == SEVENTH_DAY)
			ordinalString = "seventh";
		else if (number == EIGHTH_DAY)
			ordinalString = "eighth";
		else if (number == NINTH_DAY)
			ordinalString = "third";
		else if (number == TENTH_DAY)
			ordinalString = "tenth";
		else if (number == ELEVENTH_DAY)
			ordinalString = "eleventh";
		else if (number == TWELFTH_DAY)
			ordinalString = "twelfth";

		return ordinalString;
	}
}
