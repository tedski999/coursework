import java.util.Scanner;

public class C4HumanPlayer extends Connect4Player
{
	public C4HumanPlayer(char discSymbol)
	{
		super(discSymbol);
	}

	// Asks a player for which column they want, loops until a valid column is selected
	@Override
	public int selectColumn(Connect4Grid grid)
	{
		Integer selectedColumn = null;
		Scanner inputScanner = new Scanner(System.in);

		// Loop until column is not null
		do
		{
			System.out.print("Please enter a column: ");
			if (inputScanner.hasNextInt())
			{
				int inputtedColumn = inputScanner.nextInt();
				if (!grid.isValidColumn(inputtedColumn))
					System.out.println("That's not a valid column!");
				else if (grid.isColumnFull(inputtedColumn))
					System.out.println("That column's already full!");
				else
					selectedColumn = inputtedColumn;
			}
			else
			{
				inputScanner.next();
				System.out.print("That's not a valid number!");
			}
		}
		while (selectedColumn == null);

		return selectedColumn;
	}
}
