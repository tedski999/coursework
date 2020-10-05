// CSU11010 - Introduction to Programming 1 - Assignment 3
// Write a program to allow a user to play a game of Rock-Paper-Scissors.
// The user should be allowed to play a fixed number of times (say 5) and
// each time must be told if they win, lose or draw.  At the end a final
// score should be given.  Note that a for loop MUST be used.
// In the game the user and the computer both have to choose one of Rock,
// Paper or Scissors.  If they choose the same object then it is a draw.
// If not then one of them has won:
// Rock beats Scissors, Scissors beats Paper and Paper beats Rock.
// Ted Johnson - 04/10/19

/* SELF ASSESSMENT
	1. Did I use appropriate easy-to-understand, meaningful variables and CONSTANTS within the code?
		Mark out of 10: 10
		Comment:	Each variable and constant have descriptive names so it's easy to understand their purpose.
	2. Did I format the variable and CONSTANT names appropriate (in lowerCamelCase and UPPERCASE)?
		Mark out of 5: 5
		Comment:	Variables are formatted in lowerCamelCase and constants are UPPERCASE_WITH_UNDERSCORES.
	3. Did I generate the computer's choice in each game correctly using a Random number generator?
		Mark out of 10: 10
		Comment:	The program gets a random number between 1 and 3 inclusive from the randomNumberGenerator object,
					and then compares it with the users input to determine the round outcome.
	4. Did I input the user's choice in each game correctly?
		Mark out of 10: 10
		Comment:	The program accepts a valid integer every round as userInput.
	5. Did I correctly compare the choices and update the score appropriately?
		Mark out of 20: 20
		Comment:	Every outcome is covered using a series of if/else statements. First, the program determines if
					the round was a draw. If not, the program narrows down the outcomes until it knows who won.
					Then it increments the victors score, and prints the appropriate message to the user.
	6. Did I inform the user of who won each game (and why) correctly?
		Mark out of 10: 10
		Comment:	I used printf() to output the result of each round. It prints a different message depending on
					if the user or the computer won the last round. It also prints what the computers choice was
					and what the current round number is.
	7. Did I use an appropriate for loop to allow the player to play 5 games?  There should be only one loop.
		Mark out of 20: 20
		Comment:	I used a for loop which repeats a number of times based on the NUMBER_OF_ROUNDS constants value.
					Currently, the value is set to 5, so there are five rounds played.
	8. Did I output the final scores correctly after the 5 games were played?
		Mark out of 10: 10
		Comment:	After the for loop is complete, I used printf() again to print the two final scores. printf() allows
					me to insert the variables computerScore and userScore so the user can see the final scores.
	9. How well did I complete this self-assessment?
		Mark out of 5: 5
		Comment:	I spent a couple extra minutes improving my code based on this self-assessment, so the code is easier
					to read.
   Total Mark out of 100 (Add all the previous marks): 100
*/

import java.util.Scanner;
import java.util.Random;

public class RockPaperScissors
{
	public static final int NUMBER_OF_ROUNDS = 5;
	public static final String WIN_ROUND_MESSAGE = "You won Round %d as I chose %s%n";
	public static final String LOSE_ROUND_MESSAGE = "You lost Round %d as I chose %s%n";
	public static final String DRAW_ROUND_MESSAGE = "Round %d was a draw as I chose %s too%n";

	public static final int ROCK_VALUE = 1;
	public static final int PAPER_VALUE = 2;
	public static final int SCISSORS_VALUE = 3;

	public static void main(String[] args)
	{
		System.out.println("Welcome to the Rock, Paper, Scissors Championship!");
		Scanner inputScanner = new Scanner(System.in);
		Random randomNumberGenerator = new Random();
		int computerScore = 0;
		int userScore = 0;

		for (int roundCount = 1; roundCount <= NUMBER_OF_ROUNDS; roundCount++)
		{
			int userInput = 0;
			int computerInput = randomNumberGenerator.nextInt(SCISSORS_VALUE) + ROCK_VALUE;

			while (userInput != ROCK_VALUE && userInput != PAPER_VALUE && userInput != SCISSORS_VALUE)
			{
				System.out.print("Enter 1 (for Rock) or 2 (for Paper) or 3 (for Scissors): ");
				userInput = inputScanner.nextInt();
				if (userInput != ROCK_VALUE && userInput != PAPER_VALUE && userInput != SCISSORS_VALUE)
					System.out.println("Sorry, that number isn't  a valid choice!");
			}

			String computerChoice = "Rock";
			if (computerInput == PAPER_VALUE)
				computerChoice = "Paper";
			else if (computerInput == SCISSORS_VALUE)
				computerChoice = "Scissors";

			if (userInput == computerInput)
			{
				System.out.printf(DRAW_ROUND_MESSAGE, roundCount, computerChoice);
			}
			else
			{
				// User chose Rock
				if (userInput == ROCK_VALUE)
				{
					// Computer chose Scissors (User wins)
					if (computerInput == SCISSORS_VALUE)
					{
						userScore++;
						System.out.printf(WIN_ROUND_MESSAGE, roundCount, computerChoice);
					}
					// Computer chose Paper (User loses)
					else
					{
						computerScore++;
						System.out.printf(LOSE_ROUND_MESSAGE, roundCount, computerChoice);
					}
				}

				// User chose Paper
				else if (userInput == PAPER_VALUE)
				{
					// Computer chose Rock (User wins)
					if (computerInput == ROCK_VALUE)
					{
						userScore++;
						System.out.printf(WIN_ROUND_MESSAGE, roundCount, computerChoice);
					}
					// Computer chose Scissors (User loses)
					else
					{
						computerScore++;
						System.out.printf(LOSE_ROUND_MESSAGE, roundCount, computerChoice);
					}
				}

				// User chose Scissors
				else
				{
					// Computer chose Paper (User wins)
					if (computerInput == PAPER_VALUE)
					{
						userScore++;
						System.out.printf(WIN_ROUND_MESSAGE, roundCount, computerChoice);
					}
					// Computer chose Rock (User loses)
					else
					{
						computerScore++;
						System.out.printf(LOSE_ROUND_MESSAGE, roundCount, computerChoice);
					}
				}
			}
		}

		inputScanner.close();
		System.out.printf("%nThe final score was Computer: %d User: %d%n", computerScore, userScore);
	}
}
