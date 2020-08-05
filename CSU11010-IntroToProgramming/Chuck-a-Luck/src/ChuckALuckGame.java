
/* SELF ASSESSMENT

	1. ResolveBet

		I have correctly defined ResolveBet which takes the bet type (String) and the Wallet object, and a void return type [Mark out of 7: 7].
		Comment: ResolveBet takes a bet type as a string and the wallet object, and does not return anything. In the future, a enum may be a better alternative to a string to represent a bet type.

		My program presents the amount of cash in the wallet and asks the user how much he/she would like to bet [Mark out of 8: 8].
		Comment: The method prints the money remaining in the wallet and uses parseIntInRangeFromScanner to get how much the user wants to bet.

		My program ensures the bet amount is not greater than the cash in the wallet [Mark out of 5: 5].
		Comment: parseIntInRangeFromScanner is passed a min value of zero and a max of the money remaining in the wallet plus one, so the user can only enter a valid value, including all the money in the wallet.

		My program creates three Dice objects, rolls them and creates a total variable with a summation of the roll values returned [Mark out of 15: 15]..
		Comment: The three Die objects are created and rolled, and their returned results are summed. Creating objects to represent dice seems unnecessary; however the task calls for it.

		My program determines the winnings by comparing the bet type with the total and comparing the bet type with the dice faces for the triple bet [Mark out of 20: 20].
		Comment: I used to use a switch statement to only check if the players bet was correct. Instead, I now add all the correct bet types to an ArrayList so I can print all the winning bet types and also easily check if the user chose the correct bet type.

		My program outputs the results (win or loss) and adds the winnings to the wallet if user wins or removes the bet amount from the wallet if the user loses [Mark out of 10: 10].
		Comment: If the users bet is one of the winning bet types in the ArrayList, the user is awarded winnings. The winnings is determined by a switch table. Else, the amount the user bet is taken from the wallet.

	2. Main

		I ask the user for the amount of cash he/she has, create a Wallet object and put this cash into it [Mark out of 15: 15]
		Comment: Before the main game loop, the user is prompted to enter their starting cash, which is put into a new Wallet object.

		My program loops continuously until the user either enters quit or the cash in the wallet is 0 [Mark out of 5: 5]
		Comment: The user can enter 'quit' instead of a bet type to exit. The game also exits if the user runs out of money.

		I ask the user to enter any of the four bet types or quit [Mark out of 5: 5].
		Comment: The user can enter five options in the parseBetTypeOrQuitFromScanner method: triple, field, high, low or quit. Else, the question is asked again.

		My program calls resolveBet for each bet type entered [Mark out of 5: 5].
		Comment: After a bet type is entered (and not quit), resolveBet is called with the bet type and wallet as parameters.

		At the end of the game my program presents a summary message regarding winnings and losses [Mark out of 5: 5]
		Comment: The program prints how many rounds the user played, how much money they started with, left with and the difference between them. Custom messages are sent depending on if they lost or gained money, or if they didn't play any rounds at all.


	Total Mark out of 100 (Add all the previous marks): 100
*/


import java.util.ArrayList;
import java.util.Scanner;


public class ChuckALuckGame
{
	private static final int MAX_STARTING_MONEY = 1000000;

	private static final int FIELD_BET_LOW_MAX = 7;
	private static final int FIELD_BET_HIGH_MIN = 12;
	private static final int HIGH_BET_MIN_VALUE = 11;
	private static final int LOW_BET_MAX_VALUE = 10;

	private static final int TRIPLE_ODDS = 30;
	private static final int FIELD_ODDS = 1;
	private static final int HIGH_ODDS = 1;
	private static final int LOW_ODDS = 1;

	private static Scanner inputScanner;

	private static void resolveBet(String betType, Wallet wallet)
	{
		System.out.println("You have €" + wallet.getMoneyRemaining() + " left in your wallet.");
		int betAmount = parseIntInRangeFromScanner(0, wallet.getMoneyRemaining() + 1, "How much money are you going to bet? €");

		// Simulates three die rolls
		Die die1 = new Die();
		Die die2 = new Die();
		Die die3 = new Die();
		die1.roll();
		die2.roll();
		die3.roll();
		int totalValue = die1.getRollValue() + die2.getRollValue() + die3.getRollValue();
		System.out.println("\nThe dice show " + die1.getRollValue() + ", " + die2.getRollValue() + " and " + die3.getRollValue() + ", totaling " + totalValue + "!");

		// Determine the type of bets that would have won
		ArrayList<String> gameResults = new ArrayList<String>();
		if (totalValue <= FIELD_BET_LOW_MAX || totalValue >= FIELD_BET_HIGH_MIN)
			gameResults.add("field");
		if (die1.getRollValue() == die2.getRollValue() && die2.getRollValue() == die3.getRollValue()
			&& die1.getRollValue() != 1 && die1.getRollValue() != 6)
			gameResults.add("triple");
		else if (totalValue >= HIGH_BET_MIN_VALUE)
			gameResults.add("high");
		else if (totalValue <= LOW_BET_MAX_VALUE)
			gameResults.add("low");

		// Print what bets have won
		System.out.print("The following bets have won:");
		for (String result : gameResults)
			System.out.print(" " + result);
		System.out.println();

		// Determines if the user won anything from the bet
		if (gameResults.contains(betType))
		{
			int winnings = betAmount;
			if (betType.equals("triple"))
				winnings *= TRIPLE_ODDS;
			else if (betType.equals("field"))
				winnings *= FIELD_ODDS;
			else if (betType.equals("high"))
				winnings *= HIGH_ODDS;
			else if (betType.equals("low"))
				winnings *= LOW_ODDS;
			wallet.addMoney(winnings);
			System.out.println("You won €" + winnings + "!");
		}
		else
		{
			wallet.takeMoney(betAmount);
			System.out.println("Oh no! You lost that round...");
		}

		System.out.println();
	}

	// Returns a valid bet type inputted by the user
	private static String parseBetTypeOrQuitFromScanner(String prompt)
	{
		boolean isInputting = true;
		String response = "";
		while (isInputting)
		{
			System.out.print(prompt);
			response = inputScanner.next().toLowerCase();
			switch (response)
			{
				case "quit":
				case "triple":
				case "field":
				case "high":
				case "low":
					isInputting = false;
					break;

				default:
					System.out.println("Please enter 'quit' to exit or a valid bet - Triple, Field, High or Low!");
					break;
			}
		}
		return response;
	}

	// Returns an integer within a range inputted by the user
	private static int parseIntInRangeFromScanner(int minValue, int maxValue, String prompt)
	{
		boolean isInputting = true;
		int inputValue = 0;
		while (isInputting)
		{
			System.out.print(prompt);
			if (inputScanner.hasNextInt())
			{
				inputValue = inputScanner.nextInt();
				if (inputValue > minValue)
					if (inputValue < maxValue)
						isInputting = false;
					else
						System.out.println("Please enter a number below " + maxValue + "!");
				else
					System.out.println("Please enter a number above " + minValue + "!");
			}
			else
			{
				inputScanner.next();
				System.out.println("Please enter a valid number!");
			}
		}
		return inputValue;
	}

	public static void main(String[] args)
	{
		System.out.println("Welcome to a new game of Chuck A Luck!");
		inputScanner = new Scanner(System.in);

		int numberOfRoundsPlayed = 0;
		int startingMoney = parseIntInRangeFromScanner(0, MAX_STARTING_MONEY, "How much are you bringing to the table? €");
		Wallet wallet = new Wallet(startingMoney);

		boolean isPlaying = true;
		while (isPlaying)
		{
			// Get a response from the user and play a round or quit the game
			String response = parseBetTypeOrQuitFromScanner("Type your next bet or 'quit' to exit: ");
			if (!response.equals("quit"))
			{
				resolveBet(response, wallet);
				numberOfRoundsPlayed++;
			}
			else
				isPlaying = false;

			// Quit if the user ran out of money
			if (wallet.getMoneyRemaining() == 0)
			{
				System.out.println("You've ran out of money... The House has thrown you out!");
				isPlaying = false;
			}
		}

		// Print game results - No need if they didn't play a single round
		if (numberOfRoundsPlayed != 0)
		{
			int finalMoney = wallet.getMoneyRemaining();
			int changeInMoney = finalMoney - startingMoney;
			System.out.println("\nYou played " + numberOfRoundsPlayed + " rounds, starting with €" + startingMoney + " and leaving with €" + finalMoney + ".");

			if (finalMoney == 0)
				System.out.println("You lost all your money! I hope that wasn't your college savings...");
			else if (changeInMoney > 0)
				System.out.println("Wow! You made a profit of €" + Math.abs(changeInMoney) + "!");
			else if (changeInMoney < 0)
				System.out.println("You lost €" + Math.abs(changeInMoney) + " to the House. Better luck next time!");
			else
				System.out.println("Well, at least you didn't lose any money...");
		}
		else
			System.out.println("Maybe it was a good idea not to play...");

		inputScanner.close();
	}
}

class Wallet
{
	private int moneyRemaining;

	public Wallet(int startingAmount)
	{
		moneyRemaining = startingAmount;
	}

	public int getMoneyRemaining()
	{
		return moneyRemaining;
	}

	public void addMoney(int amount)
	{
		moneyRemaining += amount;
	}

	public void takeMoney(int amount)
	{
		moneyRemaining -= amount;
	}
}

class Die
{
	private final int SIDES_ON_DIE = 6;
	private int rollValue;

	public Die()
	{
		rollValue = 1;
	}

	public void roll()
	{
		rollValue = (int) (Math.random() * SIDES_ON_DIE + 1);
	}

	public int getRollValue()
	{
		return rollValue;
	}
}