/* SELF ASSESSMENT

Connect4Game class (35 marks): 35
My class creates references to the Connect 4 Grid and two Connect 4 Players. It asks the user whether he/she would like
to play/quit inside a loop. If the user decides to play then:
1. Connect4Grid2DArray is created using the Connect4Grid interface,
2. the two players are initialised - must specify the type to be ConnectPlayer, and
3. the game starts.
In the game, I ask the user where he/she would like to drop the piece. I perform checks by calling methods in the
Connect4Grid interface. Finally a check is performed to determine a win.
Comment:
The Connect4Game class does the above correctly. After the first game (which must be setup by the player), the user is
asked if they want to play again, setup a new game or quit completely.

Connect4Grid interface (10 marks): 10
I define all 7 methods within this interface.
Comment:
These 7 methods are defined in this interface.

Connect4Grid2DArray class (25 marks): 25
My class implements the Connect4Grid interface. It creates a grid using a 2D array. Implementation of the method to
check whether the column to drop the piece is valid. It provides as implementation of the method to check whether the
column to drop the piece is full. It provides as implementation of the method to drop the piece.  It provides as
implementation of the method to check whether there is a win.
Comment:
The implementation of each method in Connect4Grid2DArray are valid. It does use a 2D array of Connect4Players

ConnectPlayer abstract class (10 marks): 10
My class provides at lest one non-abstract method and at least one abstract method.
Comment:
I renamed ConnectPlayer to Connect4Player as I believe it may have been a typo, and this name follows the other class
names. There is two non-abstract methods (setDiskSymbol and getDiskSymbol) and one abstract method (selectColumn)

C4HumanPlayer class (10 marks): 10
My class extends the ConnectPlayer class and overrides the abstract method(s). It provides the Human player functionality.
Comment:
C4HumanPlayer overrides the abstract method and provides human player functionality with a Scanner as input.

C4RandomAIPlayer class (10 marks): 10
My class extends the ConnectPlayer class and overrides the abstract method(s). It provides AI player functionality.
Comment:
C4RandomAIPlayer overrides the abstract method and provides AI player functionality with a Random.

Total Marks out of 100: 100

*/

import java.util.Scanner;

public class Connect4Game
{
	public static final int CHAIN_LENGTH_TO_WIN = 4;
	private static final int NUMBER_OF_PLAYERS = 2;

	private Connect4Grid2DArray grid;
	private Connect4Player[] players;

	public Connect4Game()
	{
		grid = new Connect4Grid2DArray();
		players = new Connect4Player[NUMBER_OF_PLAYERS];
	}

	// Starts the program, loops until the user quits
	public void start()
	{
		System.out.println("Welcome to Connect " + CHAIN_LENGTH_TO_WIN + "!");

		// Setup and play the initial game
		setupGame();
		playGame();

		// Loops until isPlaying is false
		boolean isPlaying = true;
		do
		{
			System.out.println("Do you want to play again, setup a new game or quit?");
			System.out.print("Type 'play', 'setup', or 'quit': ");
			Scanner inputScanner = new Scanner(System.in);
			switch (inputScanner.next().toLowerCase())
			{
				case "p":
				case "play":
					playGame();
					break;

				case "s":
				case "setup":
					setupGame();
					playGame();
					break;

				case "q":
				case "quit":
					isPlaying = false;
					break;

				default:
					break;
			}
		}
		while (isPlaying);

		System.out.println("Goodbye! :)");
	}

	// Prepares a new game by asking the user for input
	private void setupGame()
	{
		System.out.println("Let's setup a new game...");

		// Initialize all the players
		for (int index = 0; index < players.length; index++)
		{
			System.out.println("\nPlayer " + (index + 1) + " Setup");
			players[index] = getNewPlayerFromUser();
		}

		System.out.println("\nOkay! Let's start!");
	}

	// Starts the Connect 4 game, loops until the game is over
	private void playGame()
	{
		int currentPlayerIndex = 0;
		boolean gameIsRunning = true;
		grid.emptyGrid();

		// Loop until gameIsRunning is false
		do
		{
			// Draw the current grid
			System.out.println(grid.toString());

			// Let the player who's turn it is play their disc
			Connect4Player currentPlayer = players[currentPlayerIndex];
			System.out.println("It's Player " + (currentPlayerIndex + 1) + "'s turn!");
			int selectedColumn = currentPlayer.selectColumn(grid);
			System.out.println("They've dropped a disc in column " + selectedColumn + "...");
			grid.dropPiece(currentPlayer, selectedColumn);

			// Did they win?
			if (grid.didLastPieceConnect4())
			{
				System.out.println("Player " + (currentPlayerIndex + 1) + " has won!");
				gameIsRunning = false;
			}
			else if (grid.isGridFull())
			{
				System.out.println("The grid is full, it's a draw!");
				gameIsRunning = false;
			}

			// Go to the next players turn
			currentPlayerIndex = (++currentPlayerIndex % NUMBER_OF_PLAYERS);
		}
		while (gameIsRunning);

		// Draw the final grid
		System.out.println(grid.toString());
	}

	// Generates a new player from user input - Changes their appearance and type
	private Connect4Player getNewPlayerFromUser()
	{
		Scanner inputScanner = new Scanner(System.in);

		// Loop until discSymbol is not null
		Character discSymbol = null;
		do
		{
			System.out.print("What should their discs look like? Please enter a single character: ");
			String input = inputScanner.next();
			if (input.length() == 1)
			{
				char character = input.charAt(0);
				if (Character.isLetterOrDigit(character))
					discSymbol = character;
				else
					System.out.println("That's not a printable character!");
			}
			else
				System.out.println("Please enter only one character!");
		}
		while (discSymbol == null);

		// Loop until newPlayer is not null
		Connect4Player newPlayer = null;
		do
		{
			System.out.print("What type of player are they? Type 'Human' or 'Robot': ");
			switch (inputScanner.next().toLowerCase())
			{
				case "h":
				case "Human":
					newPlayer = new C4HumanPlayer(discSymbol);
					break;

				case "r":
				case "robot":
					newPlayer = new C4RandomAIPlayer(discSymbol);
					break;

				default:
					System.out.println("Sorry, that's not a valid option!");
					break;
			}
		}
		while (newPlayer == null);

		// We do not close inputScanner as we will need System.in again!
		return newPlayer;
	}
}

// A static class to provide the entry point
class DriverCode
{
	// Entry point
	public static void main(String[] args)
	{
		Connect4Game game = new Connect4Game();
		game.start();
	}
}