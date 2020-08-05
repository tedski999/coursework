public class Connect4Grid2DArray implements Connect4Grid
{
	public static final int NUMBER_OF_COLUMNS = 7;
	public static final int NUMBER_OF_ROWS = 6;

	// This is a map for directions in which to check for a connect 4
	// Syntax: {columnOffsetMultiplier, rowOffsetMultiplier}
	private static final int[][] CHECK_DIRECTIONS = {
		{1, 0}, // Up / Down
		{0, 1}, // Left / Right
		{1, 1}, // UpRight / DownLeft
		{1, -1} // UpLeft / DownRight
	};

	private Connect4Player[][] gridDiscs;
	private int latestDiscColumn;
	private int latestDiscRow;
	private Connect4Player latestPlayer;

	public Connect4Grid2DArray()
	{
		latestDiscColumn = 0;
		latestDiscRow = 0;
		emptyGrid();
	}

	// Sets gridDiscs to a 2D array of null values
	public void emptyGrid()
	{
		gridDiscs = new Connect4Player[NUMBER_OF_COLUMNS][NUMBER_OF_ROWS];
	}

	// Converts the grid to a string which can be "drawn" (printed) to the console
	public String toString()
	{
		StringBuilder gridStringBuilder = new StringBuilder("\n");

		// Add the numbers on the top of the chutes
		for (int column = 0; column < NUMBER_OF_COLUMNS; column++)
			gridStringBuilder.append("   ").append(column);
		gridStringBuilder.append("\n");

		// Add the chutes on the top of the columns
		gridStringBuilder.append("+");
		for (int column = 0; column < NUMBER_OF_COLUMNS; column++)
			gridStringBuilder.append("== =");
		gridStringBuilder.append("=+\n");

		// Add all the discs in the body of the grid
		for (int row = NUMBER_OF_ROWS - 1; row >= 0; row--)
		{
			gridStringBuilder.append("| ");
			for (int column = 0; column < NUMBER_OF_COLUMNS; column++)
			{
				// Add a disc
				Connect4Player disc = gridDiscs[column][row];
				char discSymbol = (disc == null) ? ' ' : disc.getDiskSymbol(); // Empty (' ') if there's no disc here (null)
				gridStringBuilder.append("[").append(discSymbol).append("] ");
			}
			gridStringBuilder.append("|\n");
		}

		// Add the final line on the bottom
		gridStringBuilder.append("+=");
		for (int column = 0; column < NUMBER_OF_COLUMNS; column++)
			gridStringBuilder.append("====");
		gridStringBuilder.append("+\n");

		// Add the feet on the bottom
		gridStringBuilder.append("| ");
		for (int column = 0; column < NUMBER_OF_COLUMNS; column++)
			gridStringBuilder.append("    ");
		gridStringBuilder.append("|\n");

		return gridStringBuilder.toString();
	}

	// Valid columns are zero (inclusive) through NUMBER_OF_COLUMNS (exclusive)
	public boolean isValidColumn(int column)
	{
		return (column >= 0 && column < NUMBER_OF_COLUMNS);
	}

	// Valid rows are zero (inclusive) through NUMBER_OF_ROWS (exclusive)
	public boolean isValidRow(int row)
	{
		return (row >= 0 && row < NUMBER_OF_ROWS);
	}

	// If the top row of a column is set, the column is full (return true)
	public boolean isColumnFull(int column)
	{
		return (gridDiscs[column][NUMBER_OF_ROWS - 1] != null);
	}

	// Places a disc belonging to 'player' on the top of column
	public void dropPiece(Connect4Player player, int column)
	{
		// Sanity checks (this should have already been handled by the player)
		if (!isValidColumn(column) || isColumnFull(column))
		{
			System.out.println("Could not drop piece, invalid column!");
			return;
		}

		// Place the disc at the first available empty row in 'column'
		for (int row = 0; row < NUMBER_OF_ROWS; row++)
		{
			if (gridDiscs[column][row] == null)
			{
				gridDiscs[column][row] = player;
				latestDiscColumn = column;
				latestDiscRow = row;
				latestPlayer = player;
				break;
			}
		}
	}

	// Returns true if the latest piece has resulted in a win
	// Checks three discs either way in all 4 directions (vertical, horizontal and both diagonals)
	public boolean didLastPieceConnect4()
	{
		for (int direction = 0; direction < 4; direction++)
		{
			// Adds up all the discs in a chain along this direction (starts at one because the latest must be part of the chain)
			int discsInALine = 1;

			// Checks forward along the direction specified by the CHECK_DIRECTIONS map
			for (int offset = 1; offset <= Connect4Game.CHAIN_LENGTH_TO_WIN - 1; offset++)
			{
				// Gets the column and row at the latest disc position with the correct offset
				int column = latestDiscColumn + offset * CHECK_DIRECTIONS[direction][0];
				int row = latestDiscRow + offset * CHECK_DIRECTIONS[direction][1];

				// Make sure the column and row are on the grid, then check if it continues the chain
				if (isValidColumn(column) && isValidRow(row) && gridDiscs[column][row] == latestPlayer)
					discsInALine++;
				else
					break; // Don't count any further, the chain has been broken
			}

			// Checks the reverse of the direction specified by the CHECK_DIRECTIONS map
			for (int offset = 1; offset <= Connect4Game.CHAIN_LENGTH_TO_WIN - 1; offset++)
			{
				int column = latestDiscColumn - offset * CHECK_DIRECTIONS[direction][0];
				int row = latestDiscRow - offset * CHECK_DIRECTIONS[direction][1];
				if (isValidColumn(column) && isValidRow(row) && gridDiscs[column][row] == latestPlayer)
					discsInALine++;
				else
					break;
			}

			// If the chains both forwards and backwards in this direction add up to 4 or more, that means the player has won!
			if (discsInALine >= Connect4Game.CHAIN_LENGTH_TO_WIN)
				return true;
		}

		return false;
	}

	// Returns true if all the columns are full
	public boolean isGridFull()
	{
		for (int column = 0; column < NUMBER_OF_COLUMNS; column++)
			if (!isColumnFull(column))
				return false;
		return true;
	}
}
