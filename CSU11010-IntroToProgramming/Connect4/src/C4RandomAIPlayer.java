import java.util.Random;
import java.util.Scanner;


public class C4RandomAIPlayer extends Connect4Player
{
	private Random rng;

	public C4RandomAIPlayer(char discSymbol)
	{
		super(discSymbol);
		rng = new Random();
	}

	// Randomly picks a column until a valid column is selected
	@Override
	public int selectColumn(Connect4Grid grid)
	{
		Integer selectedColumn = null;

		System.out.println("Beep Boop, I'm thinking...");

		// Loops until selectedColumn is not null
		do
		{
			int randomSelection = rng.nextInt(Connect4Grid2DArray.NUMBER_OF_COLUMNS);
			if (grid.isValidColumn(randomSelection) && !grid.isColumnFull(randomSelection))
				selectedColumn = randomSelection;
		}
		while (selectedColumn == null);

		// Wait until a user presses enter
		System.out.println("Done! Hit enter to continue");
		new Scanner(System.in).nextLine();

		return selectedColumn;
	}
}
